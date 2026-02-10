import pyomo.environ as pyo
from pyomo.opt import SolverStatus, TerminationCondition, SolutionStatus
import csv
import sys
import time
import os

# command line usage:
# py ilp.py [elem|middle|high] [1|2|3]

os.makedirs("results", exist_ok=True)

t0 = time.time()

def build_and_solve(
    V, p, delta, num_schools, num_schools_lower, schools_idx, cap, y, arcs,
    solver_name="gurobi", time_limit=3600
):
    nV = len(V)                     # number of vertices/blocks
    I = range(num_schools)          # upper-level schools
    K = range(num_schools_lower)    # lower-level schools

    m = pyo.ConcreteModel()

    total_pop = sum(p)
    total_cap = sum(cap)
    cap_to_pop = total_cap / total_pop

    # --- Sets ---
    m.V = pyo.Set(initialize=V)             # vertices/blocks
    m.I = pyo.Set(initialize=list(I))       # upper-level schools
    m.K = pyo.Set(initialize=list(K))       # lower-level schools
    m.A = pyo.Set(initialize=arcs, dimen=2) # arcs for flow balance constraints

    # --- Variables ---
    m.x = pyo.Var(m.V, m.I, domain=pyo.Binary)           # assignment of vertex v to school i
    m.z = pyo.Var(m.K, m.I, domain=pyo.Binary)           # whether lower-level school k to upper-level school i is a split feeder
    m.f = pyo.Var(m.I, m.A, domain=pyo.NonNegativeReals) # flow from u to v in school zone i
    m.n = pyo.Var(m.I, domain=pyo.NonNegativeReals)      # number of vertices assigned to school i (for flow balance)

    # --- Objective ---
    def obj_rule(m):
        commute = sum(p[v] * delta[v][i] * m.x[v, i] for v in m.V for i in m.I)
        split = sum(m.z[k, i] for k in m.K for i in m.I)
        return commute + split
    m.OBJ = pyo.Objective(rule=obj_rule, sense=pyo.minimize)

    # --- Constraints ---

    # Each vertex assigned to exactly one school zone:  sum_i x_{v,i} = 1 for all v
    def assign_one_rule(m, v):
        return sum(m.x[v, i] for i in m.I) == 1
    m.AssignOne = pyo.Constraint(m.V, rule=assign_one_rule)

    # School node s_i must be assigned to itself: x_{s_i,i} = 1 for all i
    def school_self_rule(m, i):
        return m.x[schools_idx[i], i] == 1
    m.SchoolSelf = pyo.Constraint(m.I, rule=school_self_rule)

    # School node s_i cannot be assigned to other j != i: x_{s_i,j} = 0
    def school_not_other_rule(m, i, j):
        if i == j:
            return pyo.Constraint.Skip
        return m.x[schools_idx[i], j] == 0
    m.SchoolNotOther = pyo.Constraint(m.I, m.I, rule=school_not_other_rule)

    # Enrollment must be at least 50% of capacity
    def lower_rule(m, i):
        return (sum(p[v] * m.x[v, i] for v in m.V) * cap_to_pop) / cap[i] - 1 >= -0.5
    m.CapLower = pyo.Constraint(m.I, rule=lower_rule)

    # Enrollment must be at most 150% of capacity
    def upper_rule(m, i):
        return (sum(p[v] * m.x[v, i] for v in m.V) * cap_to_pop) / cap[i] - 1 <= 0.5
    m.CapUpper = pyo.Constraint(m.I, rule=upper_rule)

    # Lower-level school split constraints
    def split_rule(m, k, i):
        if num_schools_lower == 0:
            return pyo.Constraint.Skip
        rhs = sum(p[v] * y[v][k] * m.x[v, i] for v in m.V)
        lhs = 0.25 * sum(p[v] * y[v][k] for v in m.V) + total_pop * m.z[k, i]
        return lhs >= rhs
    m.Split = pyo.Constraint(m.K, m.I, rule=split_rule)

    # Zone size variable to help with flow balance rule
    def zone_size_rule(m, i):
        return m.n[i] == sum(m.x[v, i] for v in m.V)
    m.ZoneSize = pyo.Constraint(m.I, rule=zone_size_rule)

    incoming = {v: [] for v in V}
    outgoing = {v: [] for v in V}
    for (u, v) in arcs:
        outgoing[u].append((u, v))
        incoming[v].append((u, v))

    def flow_balance_rule(m, v, i):
        inflow  = sum(m.f[i, u, v] for (u, _) in incoming[v])
        outflow = sum(m.f[i, v, w] for (_, w) in outgoing[v])
        root = schools_idx[i]

        if v == root:
            # inflow - outflow = 1 - n_i  since x[root,i] = 1 and root supplies flow to all other assigned nodes
            return inflow - outflow == m.x[v, i] - m.n[i]
        else:
            # assigned nodes demand 1 unit
            return inflow - outflow == m.x[v, i]
    m.FlowBalance = pyo.Constraint(m.V, m.I, rule=flow_balance_rule)

    def flow_cap_u_rule(m, i, u, v):
        return m.f[i, u, v] <= (nV - 1) * m.x[u, i]
    m.FlowCapU = pyo.Constraint(m.I, m.A, rule=flow_cap_u_rule)

    def flow_cap_v_rule(m, i, u, v):
        return m.f[i, u, v] <= (nV - 1) * m.x[v, i]
    m.FlowCapV = pyo.Constraint(m.I, m.A, rule=flow_cap_v_rule)

    print("Built model.")

    # --- Solve ---
    t1 = time.time()
    solver = pyo.SolverFactory(solver_name)
    solver.options["TimeLimit"] = time_limit
    solver.options["MIPGap"] = 0.02
    solver.options["LogFile"] = "results/gurobi.log"

    solver.options["MIPFocus"] = 1      # prioritize feasibility
    solver.options["Heuristics"] = 0.5  # default is 0.05 → much more aggressive
    solver.options["Method"] = 1        # dual simplex (avoids barrier)
    solver.options["Presolve"] = 1      # keep presolve, but not too aggressive
    solver.options["Cuts"] = 1          # limit cut generation
    solver.options["RINS"] = 20         # enable RINS early
    solver.options["PumpPasses"] = 50   # feasibility pump (big for your case)


    result = solver.solve(m, tee=True, load_solutions=False)
    print("Solve call returned in", time.time() - t1, "sec")

    tc = result.solver.termination_condition
    status = result.solver.status

    # Termination conditions that may still have a feasible incumbent
    ACCEPTABLE_TC = {
        TerminationCondition.optimal,
        TerminationCondition.feasible,
        TerminationCondition.maxTimeLimit,
        TerminationCondition.maxIterations,
        TerminationCondition.maxEvaluations
    }

    if tc not in ACCEPTABLE_TC:
        return {
            "status": status,
            "has_solution": False,
            "termination_condition": tc,
            "objective": None,
            "assignment": None,
            "z": None,
            "model": m,
        }

    # Try to load a solution
    try:
        m.solutions.load_from(result)
    except Exception as e:
        print("Solution load failed:", e)
        return {
            "status": status,
            "has_solution": False,
            "termination_condition": tc,
            "objective": None,
            "assignment": None,
            "z": None,
            "model": m,
        }

    # --- Extract solution ---
    assign = {v: None for v in V}
    for v in V:
        for i in I:
            xv = pyo.value(m.x[v, i])
            if xv is not None and xv > 0.5:
                assign[v] = i
                break

    z_vals = {(k, i): int(round(pyo.value(m.z[k, i]))) for k in K for i in I}
    obj_val = pyo.value(m.OBJ)

    return {
        "status": status,
        "has_solution": True,
        "termination_condition": tc,
        "objective": obj_val,
        "assignment": assign,
        "z": z_vals,
        "model": m,
    }

args = sys.argv
if len(sys.argv) < 3:
    print("Usage: py ilp.py [elem|middle|high] [1|2|3]")
    sys.exit(1)

def adj_list_to_arcs(path, directed=False):
    arcs = []
    with open(path, newline="") as f:
        for row in csv.reader(f):
            u = int(row[0]); v = int(row[1])
            arcs.append((u, v))
            if not directed:
                arcs.append((v, u))
    arcs = list(dict.fromkeys(arcs))  # remove duplicates, preserve order
    return arcs

arcs = adj_list_to_arcs("data/edge_list.csv", directed=False)

V = list(range(9404))

with open("data/blocks_pop.csv", newline="") as f:
    p = [float(row[0]) for row in csv.reader(f)]

if args[1] == "elem":
    with open("data/commute_es.csv", newline="") as f:
        delta = [[float(x) for x in row] for row in csv.reader(f)]
    num_schools = 138
    num_schools_lower = 0
    with open("data/schools_es.csv", newline="") as f:
        schools_idx = [int(row[0]) for row in csv.reader(f)]
    with open("data/capacity_es.csv", newline="") as f:
        cap_list = [float(row[0]) for row in csv.reader(f)]
    y_matrix = []  # no lower-level schools

elif args[1] == "middle":
    with open("data/commute_ms.csv", newline="") as f:
        delta = [[float(x) for x in row] for row in csv.reader(f)]
    num_schools = 26
    num_schools_lower = 138
    with open("data/schools_ms.csv", newline="") as f:
        schools_idx = [int(row[0]) for row in csv.reader(f)]
    with open("data/capacity_ms.csv", newline="") as f:
        cap_list = [float(row[0]) for row in csv.reader(f)]
    with open("data/lower_es_"+args[2]+".csv", newline="") as f:
        y_matrix = [[int(x) for x in row] for row in csv.reader(f)]
elif args[1] == "high":
    with open("data/commute_hs.csv", newline="") as f:
        delta = [[float(x) for x in row] for row in csv.reader(f)]
    num_schools = 24
    num_schools_lower = 26
    with open("data/schools_hs.csv", newline="") as f:
        schools_idx = [int(row[0]) for row in csv.reader(f)]
    with open("data/capacity_hs.csv", newline="") as f:
        cap_list = [float(row[0]) for row in csv.reader(f)]
    with open("data/lower_ms_"+args[2]+".csv", newline="") as f:
        y_matrix = [[int(x) for x in row] for row in csv.reader(f)]

else:
    raise ValueError("Invalid school level. Use 'elem', 'middle', or 'high'.")

assert len(p) == len(V)
assert len(delta) == len(V)
assert len(delta[0]) == num_schools
assert len(schools_idx) == num_schools
assert len(cap_list) == num_schools
assert all(0 <= s < len(V) for s in schools_idx)
assert all(0 <= u < len(V) and 0 <= v < len(V) for (u,v) in arcs)
if num_schools_lower > 0:
    assert len(y_matrix) == len(V)
    assert len(y_matrix[0]) == num_schools_lower

sol = build_and_solve(
    V=V,
    p=p,
    delta=delta,
    num_schools=num_schools,
    num_schools_lower=num_schools_lower,
    schools_idx=schools_idx,
    cap=cap_list,
    y=y_matrix,
    arcs=arcs,
    solver_name="gurobi",
    time_limit=28800
)

print(sol["status"], sol["objective"])

outbase = f"results/{args[1]}_{args[2]}"

if sol["assignment"] is not None:
    with open(outbase + ".csv", "w", newline="") as f:
        assign = sol["assignment"]
        if any(assign[v] is None for v in V):
            raise RuntimeError("Incomplete assignment in solution.")
        row = [(assign[v] + 1) for v in range(len(V))]
        csv.writer(f).writerow(row)
else:
    # Always store something so you know what happened
    with open(outbase + "_NOSOL.txt", "w") as f:
        f.write(f"status={sol['status']}\n")
        f.write(f"termination_condition={sol['termination_condition']}\n")
        f.write("No feasible incumbent solution was found (Solution count 0 / time limit / abort).\n")
import pyomo.environ as pyo
from pyomo.environ import TerminationCondition as TC
import csv
import sys
import time

# command line usage:
# py ilp.py [elem|middle|high] [1|2|3]

t0 = time.time()

def build_and_solve(
    V, p, delta, num_schools, num_schools_lower, schools_idx, cap, y, arcs,
    solver_name="gurobi", time_limit=3600
):
    nV = len(V)
    I = range(num_schools)          # upper-level schools
    K = range(num_schools_lower)    # lower-level schools

    m = pyo.ConcreteModel()

    total_pop = sum(p)

    # --- Sets ---
    m.V = pyo.Set(initialize=V)
    m.I = pyo.Set(initialize=list(I))
    m.K = pyo.Set(initialize=list(K))
    m.A = pyo.Set(initialize=arcs, dimen=2)   # directed arcs (u,v)

    # --- Variables ---
    m.x = pyo.Var(m.V, m.I, domain=pyo.Binary)                 # x_{v,i}
    m.z = pyo.Var(m.K, m.I, domain=pyo.Binary)                 # z_{k,i}
    m.f = pyo.Var(m.I, m.A, domain=pyo.NonNegativeReals)       # f^i_{uv} >= 0

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

    # Capacity constraint: up to 50% below capacity
    def lower_rule(m, i):
        return (sum(p[v] * m.x[v, i] for v in m.V) / cap[i]) - 1 >= -0.5
    m.CapLower = pyo.Constraint(m.I, rule=lower_rule)

    # Capacity constraint: up to 50% above capacity
    def upper_rule(m, i):
        return (sum(p[v] * m.x[v, i] for v in m.V) / cap[i]) - 1 <= 0.5
    m.CapUpper = pyo.Constraint(m.I, rule=upper_rule)

    # Split-feeder style constraint:
    # 0.25 * sum_v p(v) y_{v,k} + M z_{k,i} >= sum_v p(v) x_{v,i} y_{v,k}
    def split_rule(m, k, i):
        if num_schools_lower == 0:
            return pyo.Constraint.Skip  # no lower-level schools, so skip this constraint
        rhs = sum(p[v] * y[v][k] * m.x[v, i] for v in m.V)
        lhs = 0.25 * sum(p[v] * y[v][k] for v in m.V) + total_pop * m.z[k, i]
        return lhs >= rhs
    m.Split = pyo.Constraint(m.K, m.I, rule=split_rule)

    # Flow conservation for connectivity (single-commodity flow per i):
    # sum_{(u,v)} f^i_{uv} - sum_{(v,w)} f^i_{vw} = x_{v,i} - 1[v = s_i]
    #
    # Build incoming/outgoing arc lists for speed
    incoming = {v: [] for v in V}
    outgoing = {v: [] for v in V}
    for (u, v) in arcs:
        outgoing[u].append((u, v))
        incoming[v].append((u, v))

    def flow_balance_rule(m, v, i):
        inflow = sum(m.f[i, a] for a in incoming[v])
        outflow = sum(m.f[i, a] for a in outgoing[v])
        root = schools_idx[i]
        n_i = sum(m.x[u, i] for u in m.V)  # zone size for school i

        if v == root:
            # inflow - outflow = 1 - n_i  (since x[root,i]=1)
            return inflow - outflow == m.x[v, i] - n_i
        else:
            # each assigned non-root vertex needs net inflow of 1
            return inflow - outflow == m.x[v, i]
    m.FlowBalance = pyo.Constraint(m.V, m.I, rule=flow_balance_rule)

    # f^i_{uv} <= (|V|-1) x_{u,i}
    def flow_cap_u_rule(m, i, u, v):
        return m.f[i, (u, v)] <= (nV - 1) * m.x[u, i]
    m.FlowCapU = pyo.Constraint(m.I, m.A, rule=flow_cap_u_rule)

    # f^i_{uv} <= (|V|-1) x_{v,i}
    def flow_cap_v_rule(m, i, u, v):
        return m.f[i, (u, v)] <= (nV - 1) * m.x[v, i]
    m.FlowCapV = pyo.Constraint(m.I, m.A, rule=flow_cap_v_rule)

    print("Built model in", time.time()-t0, "sec")

    # --- Solve ---
    t1 = time.time()
    solver = pyo.SolverFactory(solver_name)
    solver.options["TimeLimit"] = time_limit
    solver.options["MIPGap"] = 0.02
    solver.options["LogFile"] = "gurobi.log"

    result = solver.solve(m, tee=True)
    print("Solved in", time.time()-t1, "sec")

    term = result.solver.termination_condition
    if term not in {
        TC.infeasible,
        TC.unbounded,
        TC.infeasibleOrUnbounded,
        TC.solverFailure,
        TC.internalSolverError,
        TC.numericalError,
        TC.invalidProblem
    }:
        # --- Extract solution ---
        assign = {v: None for v in V}
        for v in V:
            # find i with x[v,i] = 1
            for i in I:
                if pyo.value(m.x[v, i]) > 0.5:
                    assign[v] = i
                    break

        z_vals = {(k, i): int(round(pyo.value(m.z[k, i]))) for k in K for i in I}
        obj_val = pyo.value(m.OBJ)

        return {
            "status": str(result.solver.termination_condition),
            "objective": obj_val,
            "assignment": assign,     # vertex -> school index i
            "z": z_vals,              # whether k->i is a split feeder
            "model": m,               # in case you want to inspect more
        }
    else:
        return {
            "status": str(term), 
            "objective": None, 
            "assignment": None, 
            "z": None, 
            "model": m
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
    time_limit=3600
)

print(sol["status"], sol["objective"])
if sol["assignment"] is None:
    raise RuntimeError(f"Solve failed: {sol['status']}")
with open("results/"+args[1]+"_"+args[2]+".csv", "w", newline="") as f:
    assign = sol["assignment"]  # dict: v -> i (0..num_schools-1)
    row = [(assign[v] + 1) for v in range(len(V))]
    writer = csv.writer(f)
    writer.writerow(row)
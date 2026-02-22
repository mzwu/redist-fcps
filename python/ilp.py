# import pyomo.environ as pyo
# from pyomo.opt import SolverStatus, TerminationCondition, SolutionStatus
import gurobipy as gp
from gurobipy import GRB
import networkx as nx
from gerrychain import Graph
import csv
import sys
import time
import os

# command line usage:
# py ilp.py [elem|middle|high] [1|2|3] [time_limit]

os.makedirs("results", exist_ok=True)

t0 = time.time()

def build_and_solve(
    G, delta, num_schools, num_schools_lower, schools_idx, cap, feeder, time_limit=3600
):
    nV = G.number_of_nodes()        # number of blocks/vertices
    I = range(num_schools)          # upper-level schools
    K = range(num_schools_lower)    # lower-level schools

    m = gp.Model()
    m.Params.TimeLimit = time_limit

    total_pop = sum(G.nodes[node]['pop'] for node in G.nodes)
    total_cap = sum(cap)
    cap_to_pop = total_cap / total_pop

    # --- Variables ---
    x = m.addVars(G.nodes, I, vtype=GRB.BINARY)    # x[v,i] equals one when block v is assigned to attendance area i
    y = m.addVars(G.edges, vtype=GRB.BINARY)       # y[u,v] equals one when edge {u,v} is cut
    z = m.addVars(K, I, vtype=GRB.BINARY)          # z[k,i] equals one when lower-level school k to upper-level school i is a split feeder
    r = m.addVars(G.nodes, I, vtype=GRB.BINARY)    # r[v,i] equals one if block v is the "root" of district i
    DG = nx.DiGraph(G) # directed version of G
    f = m.addVars(DG.edges, vtype=GRB.CONTINUOUS)  # f[u,v] = amount of flow sent across arc (u,v)

    # --- Objective ---
    commute = gp.quicksum(G.nodes[v]["pop"] * delta[v][i] * x[v, i] for v in G.nodes for i in I)
    split = gp.quicksum(z[k, i] for k in K for i in I) if num_schools_lower > 0 else 0
    m.setObjective(commute + split, GRB.MINIMIZE)

    # --- Constraints ---
    # Each vertex assigned to exactly 1 attendance area
    m.addConstrs(gp.quicksum(x[v,i] for i in I) == 1 for v in G.nodes)
    
    # School node s_i must be assigned to itself
    m.addConstrs(x[schools_idx[i], i] == 1 for i in I)
    
    # School node s_i cannot be assigned to another j
    m.addConstrs(x[schools_idx[i], j] == 0 for i in I for j in I if i != j)
    
    # Enrollment must be at least 50% of capacity
    m.addConstrs(gp.quicksum(G.nodes[v]["pop"] * x[v,i] for v in G.nodes) * cap_to_pop / cap[i] - 1 >= -0.5 for i in I)
    
    # Enrollment must be at most 150% of capacity
    m.addConstrs(gp.quicksum(G.nodes[v]["pop"] * x[v,i] for v in G.nodes) * cap_to_pop / cap[i] - 1 <= 0.5 for i in I)
    
    # Lower-level school split constraints
    m.addConstrs(gp.quicksum(G.nodes[v]["pop"] * feeder[v][k] * x[v,i] for v in G.nodes) <= 0.25 * gp.quicksum(G.nodes[v]["pop"] * feeder[v][k] for v in G.nodes) + total_pop * z[k,i] for k in K for i in I)

    # Edge {u,v} is cut if u is assigned to district i but v is not
    m.addConstrs(x[u,i] - x[v,i] <= y[u,v] for u,v in G.edges for i in I)

    # Flow constraints
    M = G.number_of_nodes() - len(I) + 1
    # Each district should have 1 root
    m.addConstrs(gp.quicksum(r[v,i] for v in DG.nodes) == 1 for i in I)
    # If node v is not assigned to attendance area i, then it cannot be its root
    m.addConstrs(r[v,i] <= x[v,i] for v in DG.nodes for i in I)
    # If not a root, consume some flow
    # If a root, only send out some flow
    m.addConstrs(gp.quicksum(f[u,v] - f[v,u] for u in DG.neighbors(v)) >= 1 - M * gp.quicksum(r[v,i] for i in I) for v in G.nodes)
    # Do not send flow across cut edges
    m.addConstrs(f[u,v] + f[v,u] <= M * (1 - y[u,v]) for (u,v) in G.edges)

    print("Built model.")

    # --- Solve ---
    m.update()
    m.optimize()

    if m.SolCount <= 0:
        print("No feasible solution found.")
        return {
            "solution_found": False,
            "objective": -1,
            "assignment": None
        }

    return {
        "solution_found": True,
        "objective": m.ObjVal,
        "assignment": m.getAttr("X", x)
    }

args = sys.argv
if len(sys.argv) < 4:
    print("Usage: py ilp.py [elem|middle|high] [1|2|3] [time_limit]")
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
    feeder = []  # no lower-level schools

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
        feeder = [[int(x) for x in row] for row in csv.reader(f)]
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
        feeder = [[int(x) for x in row] for row in csv.reader(f)]

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
    assert len(feeder) == len(V)
    assert len(feeder[0]) == num_schools_lower

G_nx = nx.Graph()
G_nx.add_edges_from(arcs)

for v in V:
    G_nx.nodes[v]["pop"] = p[v]

G = Graph.from_networkx(G_nx)

sol = build_and_solve(
    G=G,                                  # graph: nodes, edges, populations
    delta=delta,                          # commute times: n_blocks x n_schools
    num_schools=num_schools,              # number of schools/attendance areas
    num_schools_lower=num_schools_lower,  # number of schools in lower level
    schools_idx=schools_idx,              # which blocks each school corresponds to
    cap=cap_list,                         # capacities of each school
    feeder=feeder,                        # which lower level school each blocks was assigned to
    time_limit=float(args[3])
)

if sol["solution_found"]:
    x_sol = sol["assignment"]
    assignment = {}
    for v in G.nodes:
        for i in range(num_schools):
            if x_sol[v,i] > 0.5:
                assignment[v] = i
                break
    outbase = f"results/{args[1]}_{args[2]}_{args[3]}"
    with open(outbase + ".csv", "w", newline="") as f:
        row = [(assignment[v] + 1) for v in range(len(V))]
        csv.writer(f).writerow(row)

print(sol["solution_found"], sol["objective"], sol["assignment"])
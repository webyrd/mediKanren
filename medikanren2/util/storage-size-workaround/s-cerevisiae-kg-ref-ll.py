
import os

rfile_nodes = "medikanren2/data/sri-reference/0.3.0/upstream-tsv-nocr/nodes.tsv"
rfile_edges = "medikanren2/data/sri-reference/0.3.0/upstream-tsv-nocr/edges.tsv"
# When used as configured, requires ~9GB RAM ("RES = Resident Size" in top)

configs = {
    'small': {
        'level_max': 4,
        'rel_blacklist':{
            "biolink:related_to":1,
            "biolink:actively_involved_in":1,
            "biolink:enables":1,
            "biolink:part_of":1
        },
        'prefix_output': 'medikanren2/data/yeast-sri-reference/0.3.0b/tsv'
    },
    'micro': {
        'level_max': 0,
        'rel_blacklist':{
            "biolink:related_to":1,
            "biolink:actively_involved_in":1,
            "biolink:enables":1,
            "biolink:part_of":1
        },
        'prefix_output': 'medikanren2/data/yeast-micro-sri-reference/0.3.0b/tsv'
    }
}

kconfig='small'
do_write = True

level_max = configs[kconfig]['level_max']
rel_blacklist = configs[kconfig]['rel_blacklist']
prefix_output = configs[kconfig]['prefix_output']

def dictappend(d,k,v):
    if not k in d:
        d[k]=[v]
    else:
        vs=d[k]
        vs.append(v)
        d[k]=vs

nodes = {}

edges_fw = {}
edges_rev = {}

cNodes=0
with open(rfile_nodes) as fin: 
    iline=0
    for line in fin:
        iline += 1
        if iline == 1:
            continue # skip header
        cNodes += 1
        xs = line.split("\t")
        nodeid = xs[0]
        if nodeid.startswith("SGD"):
            node = 0
            nodes[nodeid]=node

print("{} of {} nodes are SGD".format(len(nodes), cNodes))

with open(rfile_edges) as fin: 
    iline=0
    for line in fin:
        iline += 1
        if iline == 1:
            continue # skip header
        xs = line.split("\t")
        srcid = xs[1]
        destid = xs[3]
        rel = xs[2]
        if not rel in rel_blacklist:
            dictappend(edges_fw, srcid, (destid, rel))
            dictappend(edges_rev, destid, (srcid, rel))


cFw=0
cRev=0
for level in range(0,level_max+1):
    print("SGD level {} has {} nodes ({} found searching forward, {} found searching reverse)".format(level, len(nodes), cFw, cRev))
    cFw=0
    cRev=0
    if level < level_max:
        for nodeid1 in list(nodes.keys()):
            for edge in edges_fw[nodeid1] if nodeid1 in edges_fw else []:
                nodeid2 = edge[0]
                if not (nodeid2 in nodes):
                    nodes[nodeid2] = level
                    cFw += 1
                    # if (cFw % 1024) == 0:
                    #     print("cFw={} rel={}".format(cFw, edge[1]))
            for edge in edges_rev[nodeid1] if nodeid1 in edges_rev else []:
                nodeid2 = edge[0]
                if not (nodeid2 in nodes):
                    nodes[nodeid2] = level
                    cRev += 1
                    # if (cRev % 1024) == 0:
                    #     print("cRev={} rel={}".format(cRev, edge[1]))

def path_prefix(prefix,rfile):
    return os.path.join(prefix, os.path.basename(rfile))

if do_write:
    os.makedirs(prefix_output, exist_ok=True)
    with open(rfile_nodes) as fin: 
        with open(path_prefix(prefix_output, rfile_nodes), "w") as fout:
            fout.seek(0)
            fout.truncate()
            iline=0
            for line in fin:
                iline += 1
                if iline == 1:
                    fout.write(line)
                else:
                    cNodes += 1
                    xs = line.split("\t")
                    nodeid = xs[0]
                    if nodeid in nodes:
                        fout.write(line)

    with open(rfile_edges) as fin: 
        with open(path_prefix(prefix_output, rfile_edges), "w") as fout:
            fout.seek(0)
            fout.truncate()
            iline=0
            for line in fin:
                iline += 1
                if iline == 1:
                    fout.write(line)
                else:
                    xs = line.split("\t")
                    srcid = xs[1]
                    destid = xs[3]
                    rel = xs[2]
                    if srcid in nodes and destid in nodes:
                        fout.write(line)




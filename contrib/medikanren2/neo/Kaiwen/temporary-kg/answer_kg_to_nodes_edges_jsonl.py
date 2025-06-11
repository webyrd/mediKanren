import requests
import json

# Load TRAPI JSON
url = "https://answerkg.metareflective.systems/NAGPA.json"
response = requests.get(url)
data = response.json()

# Main TRAPI message
message = data["fields"]["data"]["message"]
kg_nodes = message["knowledge_graph"]["nodes"]
kg_edges = message["knowledge_graph"]["edges"]

# Helper: check for creative edge
def is_creative_edge(edge):
    for attr in edge.get("attributes", []):
        if attr.get("attribute_type_id") == "biolink:support_graphs":
            return True
    return False

# Flatten attribute_type_id:value from edge['attributes']
def flatten_attributes(attributes):
    flat = {}
    for attr in attributes or []:
        key = attr.get("attribute_type_id")
        value = attr.get("value")
        if key:
            flat[key] = value
    return flat

# Flatten qualifier_type_id:qualifier_value
def flatten_qualifiers(qualifiers):
    flat = {}
    for q in qualifiers or []:
        key = q.get("qualifier_type_id")
        value = q.get("qualifier_value")
        if key:
            flat[key] = value
    return flat

# Extract primary knowledge source
def extract_primary_source(sources):
    for source in sources or []:
        if source.get("resource_role") == "primary_knowledge_source":
            return {"primary_knowledge_source": source.get("resource_id")}
    return {}

# Extract real edges only
real_edges = {
    edge_id: edge_info
    for edge_id, edge_info in kg_edges.items()
    if not is_creative_edge(edge_info)
}

# Write nodes.jsonl
with open("nodes.jsonl", "w") as nf:
    for node_id, node_info in kg_nodes.items():
        output = {
            "id": node_id,
            "name": node_info.get("name"),
            "categories": node_info.get("categories")
        }
        nf.write(json.dumps(output) + "\n")

# Write enriched edges.jsonl
with open("edges.jsonl", "w") as ef:
    for edge_id, edge_info in real_edges.items():
        edge_output = {
            "id": edge_id,
            "subject": edge_info.get("subject"),
            "object": edge_info.get("object"),
            "predicate": edge_info.get("predicate")
        }

        # Merge in flattened attributes, qualifiers, and primary source
        edge_output.update(flatten_attributes(edge_info.get("attributes")))
        edge_output.update(flatten_qualifiers(edge_info.get("qualifiers")))
        edge_output.update(extract_primary_source(edge_info.get("sources")))

        ef.write(json.dumps(edge_output) + "\n")

print("Extraction complete: 'nodes.jsonl' and 'edges.jsonl' created.")

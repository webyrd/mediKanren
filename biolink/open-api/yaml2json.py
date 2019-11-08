import sys, json, yaml

data = yaml.load(sys.stdin, Loader=yaml.SafeLoader)
print(json.dumps(data, indent=4))

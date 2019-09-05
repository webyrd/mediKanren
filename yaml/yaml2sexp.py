def sexp(r, indent=0, dot='. '):
  i = indent*' '
  j = (indent+2)*' '
  if type(r) == dict:
    print(i+'(')
    for k,v in r.items():
      print(j+'(')
      sexp(k, indent+4, '')
      sexp(v, indent+4)
      print(j+')')
    print(i+')')
  elif type(r) == list:
    print(i+'(')
    for x in r:
      sexp(x,indent+2,'')
    print(i+')')
  elif type(r) == bool:
    if r:
      print(i+dot+'#t')
    else:
      print(i+dot+'#f')
  else:
    print(i+dot+'"'+r+'"')

if __name__ == '__main__':
  from ruamel.yaml import YAML
  yaml=YAML(typ='safe')   # default, if not specfied, is 'rt' (round-trip)
  import sys
  m = 'example1' if len(sys.argv)<2 else sys.argv[1]
  r = yaml.load(open(m+'.yaml'))
  print('(define '+m)
  print("'", end='')
  sexp(r)
  print(')')


The data can be generated with these commands:

```
racket csv-semmed-ordered-unique-enum.rkt semmedVER30_A_clean.csv semmed

racket csv-semmed-simplify.rkt semmedVER30_A_clean.csv semmed
```


NOTE:

The strange 1176 predicate might be legitimate, found in a couple records via:

```
tail -n 2318424 semmedVER30_A_clean.csv | head -n 118424 | grep 1176 | less
78924979,136605831,11116842,1176,C0024485,Magnetic Resonance Imaging,diap,1,C3146964,COX5BP4 gene,0,1
78924980,136605831,11116842,1176,C0024485,Magnetic Resonance Imaging,diap,1,C3146964,COX5BP4 gene,0,1
```

The strange 0 semtype might be legitimate, found alongside predicate 1176.


TODO:

concept ideas:
  cui-by-semtype

edge ideas:
  edge, edge-reversed: #(cui cui detail-offset)
    Byte-level encode so that each record is at a predictable offset.
    Write this out in flattened search-tree form for fast lookup; store offset into edge-details.

  edge-details (and reversed cui version): #(cui cui (list-of #(predicate semtype semtype predication-id)))

# VIS.JS Instructions
After using make-map.rkt to create the DOT graph for whichever knowledge graph you would like to visualize, it is then possible to visualize this DOT graph using Vis.js. 

In order to do so, create a new html file and add the following to the header: 

```
<head>
  <title>[Your Title Here]</title>

  <script src="https://unpkg.com/vis-network/standalone/umd/vis-network.min.js"></script>
</head>
```


Then, to insert the DOT graph, do the following:
```
<div id="mynetwork"></div>
<script type="text/javascript">
    var container = document.getElementById('mynetwork');
    var dot = "INSERT DOT GRAPH STRING COPIED FROM DOT FILE HERE";
    var data = vis.parseDOTNetwork(dot);
    var options = {};
    var network = new vis.Network(container, data, options);
</script>
```
Then, close the body of the html file and display its contents to navigate the graph. For the options variable, there is documentation on possible options at this link: https://visjs.github.io/vis-network/docs/network/#options

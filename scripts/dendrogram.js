var outputBinding = new Shiny.OutputBinding();
$.extend(outputBinding, {
  find: function(scope) {
    return $(scope).find('.d3graph');
  },
  renderValue: function(el, data) {
    wrapper(el, data);
  }});
Shiny.outputBindings.register(outputBinding);

function wrapper(el, data) {
  var margin = {top: 10, bottom: 10, left: 25, right: 10},
      width = $(window).width() - margin.right - margin.left,
      height = $(window).height() - $('.span12').height() - 50 - $('.nav-tabs').height() - $('.navbar').height();
  
  var i = 0,
    duration = 750,
    root;

  var cluster = d3.layout.cluster()
      .size([height - margin.bottom, width - 60]);

  var diagonal = d3.svg.diagonal()
      .projection(function(d) { return [d.y, d.x]; });

  d3.select(el).select("svg").remove();
  var svg = d3.select(el).append("svg")
      .attr("width", width)
      .attr("height", height)
    .append("g")
      .attr("transform", "translate(" + margin.left + "," + margin.top + ")");
  
  if(data) { // wait for data to load
      console.log(data);
      root = JSON.parse(data);
      console.log(root);
      root.x0 = height / 2;
      root.y0 = root.height;
      

      //rescale heights
      var x_map = d3.scale.linear()
        .domain([0, root.height])
        .range([width - 60, 0]);
      
      // collapse children and draw tree
      root.children.forEach(collapse);
      update(root);

  }

  function update(source) {
    // Compute the new cluster layout.
    var nodes = cluster.nodes(root).reverse(),
        links = cluster.links(nodes);

    // Normalize for fixed-depth.
    nodes.forEach(function(d) { d.y = d.depth * 180; });

    // Update the nodes…
    var node = svg.selectAll("g.node")
        .data(nodes, function(d) { return d.id || (d.id = ++i); });

    // Enter any new nodes at the parent's previous position.
    var nodeEnter = node.enter().append("g")
        .attr("class", "node")
        .attr("transform", function(d) { return "translate(" + x_map(source.y0) + "," + source.x0 + ")"; })
        .on("click", click);

    nodeEnter.append("circle")
        .attr("r", 1e-6)
        .style("fill", function(d) { return d._children ? "lightsteelblue" : "#fff"; });

    nodeEnter.append("text")
        .attr("x", function(d) { return d.children || d._children ? -8 : 8; })
        .attr("dy", ".35em")
        .attr("text-anchor", function(d) { return d.children || d._children ? "end" : "start"; })
        .text(function(d) { return d.name; })
        .style("fill-opacity", 1e-6);

    // Transition nodes to their new position.
    var nodeUpdate = node.transition()
        .duration(duration)
        .attr("transform", function(d) { return "translate(" + x_map(d.height) + "," + d.x + ")"; });
    
    nodeUpdate.select("circle")
        .attr("r", 4.5)
        .style("fill", function(d) { return d._children ? "lightsteelblue" : "#fff"; });

    
    nodeUpdate.select("text")
        .style("fill-opacity", 1);

    // Transition exiting nodes to the parent's new position.
    var nodeExit = node.exit().transition()
        .duration(duration)
        .attr("transform", function(d) { return "translate(" + x_map(source.height) + "," + source.x + ")"; })
        .remove();

    nodeExit.select("circle")
        .attr("r", 1e-6);

    nodeExit.select("text")
        .style("fill-opacity", 1e-6);

    // Update the links…
    var link = svg.selectAll("path.link")
        .data(links, function(d) { return d.target.id; });

    // Enter any new links at the parent's previous position.
    link.enter().insert("path", "g")
        .attr("class", "link")
        .attr("d", function(d) {
          return "M" + x_map(source.y0) + "," + source.x0
            + "L" + x_map(source.y0) + "," + source.x0;
      });
    

    // Transition links to their new position.
    link.transition()
        .duration(duration)
        .attr("d", function(d) {
          return "M" + x_map(d.target.height) + "," + d.target.x
            + "L" + x_map(d.source.height) + "," + d.target.x
            + " " + x_map(d.source.height) + "," + d.source.x;
      });

    // Transition exiting nodes to the parent's new position.
    link.exit().transition()
        .duration(duration)
        .attr("d", function(d) {
          return "M" + x_map(d.source.height) + "," + d.source.x
            + "L" + x_map(d.source.height) + "," + d.source.x;
        })
        .remove();

    // Stash the old positions for transition.
    nodes.forEach(function(d) {
      d.x0 = d.x;
      d.y0 = d.height;
    });
  }

  // Toggle children on click.
  function click(d) {
    if (d.children) {
      d._children = d.children;
      d.children = null;
    } else {
      d.children = d._children;
      d._children = null;
    }
    update(d);
  }
  
  // collapse childen nodes  
  function collapse(d) {
    if (d.children) {
      d._children = d.children;
      d._children.forEach(collapse);
      d.children = null;
    }
  }
  
}



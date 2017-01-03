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

  d3.select(el).select("svg").remove();
  var margin = {top: 10, bottom: 10, left: 25, right: 10},
      width = $(window).width() - margin.right - margin.left,
      height = 2000 - margin.top - margin.bottom;

  var tree = d3.tree()
      .size([height - 10, width - 60]);

  if(data) {
    // wait for data to load
    
    // create hierarchy
    var root = d3.hierarchy(data, function(d) { return d.children; });
    
    // calculate cumulative heights
    //root.sum(function(d) { return d.height});
    
    console.log(root.leaves());
     
    
        
    //rescale heights
    var x_map = d3.scaleLinear()
      .domain([0, root.data.height])
      .range([width - 60, 0]);

    
    // create dendro object  
    tree(root);
    
    // append svg to page
    d3.select(el).select("svg").remove();
    var g = d3.select(el).append("svg")
        .attr("width", width)
        .attr("height", height)
      .append("g")
        .attr("transform", "translate(" + margin.left + "," + margin.top + ")");
    
    // append links
    var link = g.selectAll(".link")
        .data(root.descendants().slice(1))
      .enter().append("path")
        .attr("class", "link")
        .attr("d", function(d) {
          return "M" + x_map(d.data.height) + "," + d.x
            + "L" + x_map(d.parent.data.height) + "," + d.x
            + " " + x_map(d.parent.data.height) + "," + d.parent.x;
      });
    
    // append nodes
    var node = g.selectAll(".node")
        .data(root.descendants())
      .enter().append("g")
        .attr("class", function(d) { return "node" + (d.children ? " node--internal" : " node--leaf"); })
        .attr("transform", function(d) { return "translate(" + x_map(d.data.height) + "," + d.x + ")"; });

    node.append("circle")
        .attr("r", 2.5);

    node.append("text")
        .attr("dy", 3)
        .attr("x", function(d) { return d.children ? -8 : 8; })
        .style("text-anchor", function(d) { return d.children ? "end" : "start"; })
        .text(function(d) { return d.data.name; }); 
  }
  

}
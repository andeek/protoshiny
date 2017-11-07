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
  var margin = {top: 10, bottom: 5, left: 0, right: 10},
      height = $(window).height() - $('.span12').height() - $('.nav-tabs').height() - $('.navbar').height() - margin.bottom - margin.top;
    
  var i = 0,
    duration = 750,
    root;
  
  if(data) { // wait for data to load
      root = JSON.parse(data.data);
      root.x0 = height / 2;
      root.y0 = root.height;
      
      var maxLabelLength = 0;
      var leaves = [];
      visit(root, function(d) {
        maxLabelLength = Math.max(d.name.length, maxLabelLength);
		    if (!d.children && !d._children) {leaves.push(d);}
      }, function(d) {
        if (d.children) {return d.children;}
		    else if (d._children) {return d._children;}
		    else {return null;}
      });
      
      
      var right_label_pad = maxLabelLength*7 + 12,
          left_label_pad = root.name.length*7 + 10,
          slider_pad = 50,
          width = $(window).width() - margin.right - margin.left;
          

      var cluster = d3.layout.cluster()
          .size([height - slider_pad - margin.bottom, width - right_label_pad - left_label_pad]);


      var diagonal = d3.svg.diagonal()
          .projection(function(d) { return [d.y, d.x]; });
    
      d3.select(el).select("svg").remove();
      var svg = d3.select(el).append("svg")
          .attr("width", width)
          .attr("height", height)
        .append("g")
          .attr("transform", "translate(" + left_label_pad + "," + margin.top + ")");
          
      //slider stuff
      var slider = svg.append("g")
        .attr("class", "slider")
        .attr("transform", "translate(0, " + (height - slider_pad - margin.bottom) + ")");
        
      var max_height = 0;

      // rescale heights
      max_height = root.height;
      var x_map = d3.scale.linear()
        .domain([0, max_height])
        .range([width - right_label_pad - left_label_pad, 0]);
      
      // slider scale
      var slide_x = d3.scale.linear()
        .domain([root.height, 0])
        .range([0, width - right_label_pad - left_label_pad])
        .clamp(true);
        
      var drag = d3.behavior.drag();
      
      // slider axis  
      slider.append("line")
          .attr("class", "track")
          .attr("x1", slide_x.range()[0])
          .attr("x2", slide_x.range()[1])
        .select(function() { return this.parentNode.appendChild(this.cloneNode(true)); })
          .attr("class", "track-inset")
        .select(function() { return this.parentNode.appendChild(this.cloneNode(true)); })
          .attr("class", "track-middle")
        .select(function() { return this.parentNode.appendChild(this.cloneNode(true)); })
          .attr("class", "track-overlay");
          
          
        
      slider.insert("g", ".track-overlay")
          .attr("class", "ticks")
          .attr("transform", "translate(0," + 18 + ")")
        .selectAll("text")
        .data(slide_x.ticks(10))
        .enter().append("text")
          .attr("x", slide_x)
          .attr("text-anchor", "middle")
          .text(function(d) { return d; });
      
      // slider handles
      var handle_max = slider.append("circle", ".track-overlay")
          .attr("class", "handle max")
          .attr("r", 9)
          .attr("cx", slide_x.range()[1])
          .call(drag.on("drag", zoomed));
          
      var handle_min = slider.append("circle", ".track-overlay")
          .attr("class", "handle min")
          .attr("r", 9)
          .attr("cx", slide_x.range()[0])
          .call(drag.on("drag", zoomed));
      
      // collapse children and draw tree
      if(data.path) {
        collapse(root);
        root = nav_path(data.path, root);
      } else {
        root.children.forEach(function(d, i){ 
          d.children.forEach(function(d, i){ 
            d.children.forEach(collapse); 
          }); 
        }); 
      }
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
  
  // expand childen nodes (1 level)  
  function expand(d) {
    if (d._children) {
      d.children = d._children;
      d._children = null;
    }
  }
  
  function zoomed(d) {
    var x_val = slide_x.invert(d3.event.x);
    var right, left;
    
    console.log(x_val);
    
    if(d3.select(this).attr("class") == "handle max") {
      var min_val = slide_x.invert(handle_min.attr("cx"));
      if(x_val >= min_val - 0.005) x_val = min_val - 0.005;
      
      // move slider
      handle_max.attr("cx", slide_x(x_val));
      
      d3.select(".track-middle")
        .attr("x2", slide_x(x_val));
      
      // zoom
      x_map.domain([x_val, min_val]);
      
    } else if(d3.select(this).attr("class") == "handle min") {
        
      left = x_val;
      if(left === max_height) left = max_height - 0.0001;
      
      var max_val = slide_x.invert(handle_max.attr("cx"));
      if(left <= max_val + 0.005) left = max_val + 0.005;
      
      // move slider
      handle_min.attr("cx", slide_x(left));
      
      d3.select(".track-middle")
        .attr("x1", slide_x(left));
      
      // zoom
      x_map.domain([max_val, left]);
    }
    update(root);
  }
  
  function visit(parent, visitFn, childrenFn) {
      if (!parent) return;
  
      visitFn(parent);
  
      var children = childrenFn(parent);
      if (children) {
          var count = children.length;
          for (var i = 0; i < count; i++) {
              visit(children[i], visitFn, childrenFn);
          }
      }
  }
  
  // navigate the tree to a specific value
  function nav_path(path, data) {
    var path_vec = path.split(",");
    var dat = data;
    
    // expand first level
    expand(dat);

    //recursively expand the correct children
    inner_nav(path_vec, dat);
    
    return(dat);
    
  }
  
  // inner function for nav_path to make recursion happen
  function inner_nav(path, data) {
    console.log(path);
    console.log(data);
    
    if (path.length > 1) {
      expand(data.children[+path[0]]);
      inner_nav(path.slice(1), data.children[+path[0]]);
    }
  }
}



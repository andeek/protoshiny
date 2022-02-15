
var outputBinding = new Shiny.OutputBinding();
$.extend(outputBinding, {
  find: function(scope) {
    return $(scope).find('.d3graph');
  },
  renderValue: function(el, data) {
    wrapper(el, data);
  }});
Shiny.outputBindings.register(outputBinding);

var clusters_return = "???";

// TODO: rewrite as a function that encodes 0 and 1s 
function get_nodes_children(obj) {
  var n = obj.length;
  var id = Array.apply(null, Array(n));
  var label = Array.apply(null, Array(n));
  var merge_id = Array.apply(null, Array(n));
  var merge_height = Array.apply(null, Array(n));
  var terminal = Array.apply(null, Array(n));
  for(i=0; i < obj.length; i++) {
    var obj_i = obj[i];
    id[i] = obj_i.id;
    label[i] = obj_i.name;
    merge_id[i] = obj_i.merge_id
    merge_height[i] = obj_i.height
    terminal[i] = obj_i._children ? true : false;
  }
  var res = {id: id, label: label, merge_id: merge_id, merge_height: merge_height, terminal: terminal};
  return(res);
}

var inputBinding = new Shiny.InputBinding();
$.extend(inputBinding, {
  find: function(scope) {
    return $(scope).find('#download');
  },
  getValue: function(el) {
    return get_nodes_children(clusters_return);
  },
  subscribe: function(el, callback) {
    $(el).on("click", function(e) {
      callback();
    });
  },
});
Shiny.inputBindings.register(inputBinding);

function wrapper(el, data) {
  console.log("in the wrapper");
  
  var margin = {top: 10, bottom: 5, left: 5, right: 5},
      height = window.innerHeight - $('.nav-tabs').height() - $('.navbar').height() - margin.bottom - margin.top,
      zoom_scale = 1;

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
      
      var right_label_pad = 100,
          left_label_pad = root.name.length*5 + 10,
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
          .call(d3.behavior.zoom().on("zoom", zoom))
        .append("g")
          .attr("transform", "translate(" + left_label_pad + "," + margin.top + ")");
      
      var max_height = 0;

      // rescale heights
      max_height = root.height;
      var x_map = d3.scale.linear()
        .domain([0, max_height])
        .range([width - right_label_pad - left_label_pad, 0]);
        
      //slider stuff
      var slider = svg.append("g")
        .attr("class", "slider")
        .attr("transform", "translate(0, " + (height - slider_pad - margin.bottom) + ")");
      
      // slider scale
      var slide_x = d3.scale.linear()
        // .domain([root.height, d3.format(".1n")(-d3.max([maxLabelLength - 100, 0])/100) - 0.1])
        .domain([root.height, d3.format(".1n")(-d3.max([maxLabelLength - 100, 0])/100)])
        .range([0, width - right_label_pad - left_label_pad])
        .clamp(true);
        
      // slider axis  
      slider.append("line")
          .attr("class", "track")
          .attr("x1", slide_x.range()[0])
          .attr("x2", slide_x.range()[1])
        .select(function() { return this.parentNode.appendChild(this.cloneNode(true)); })
          .attr("class", "track-inset");
          
      slider.append("line")
          .attr("class", "track")
          .attr("x1", slide_x.range()[0])
          .attr("x2", slide_x(0))
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
          .attr("class", "slider-label")
          .attr("x", slide_x)
          .attr("text-anchor", "middle")
          .text(function(d) { return d; });
      
      // collapse children and draw tree
      console.log(data.path);
      if(data.path) {
        var len = 0;
        collapse(root);
        if(typeof data.path == 'string') {
          // there is only one path to follow
          root = nav_path(data.path, root);
        } else if(data.path.length === 0) {
          // dynamic tree cut returns null....
          // do nothing?
        } else if(typeof data.path == 'object') {
          for(i in data.path) root = nav_path(data.path[i], root);
        }
      } else {
        if(root.children) {
          root.children.forEach(function(d, i){
            if(d.children) {
              d.children.forEach(function(d, i){
                if(d.children) {
                  d.children.forEach(collapse); 
                }
              }); 
            }
          });
        }
      }
      update(root);

  }

  function update(source) {
    // Compute the new cluster layout.
    var nodes = cluster.nodes(root).reverse(),
        links = cluster.links(nodes);
        
    clusters_return = nodes; 
    

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
        .style("fill", function(d) { return d._children ? "lightsteelblue" : "#fff"; })
        .attr("transform", "scale(" + 1/zoom_scale + ")");

    if(data.img_path) {
      var imageEnter = nodeEnter.append("image")
        .attr("xlink:href", function(d) { 
          if(d.parent) {
            // not the root
            if(d.children || d._children) {
              // not the leaves
              return d.parent.name != d.name ? data.img_path_loc + "/" + d.img : "";
            } else {
              //leaves
              return data.img_path_loc + "/" + d.img;
            }
          } else {
            // root
            return data.img_path_loc + "/" + d.img;
          }
        })
        .attr("x", "-25px")
        .attr("y", "-25px")
        .attr("width", "50px")
        .attr("height", "50px")
        .attr("class", function(d) {return d._children ? "label-child" : "label-nochild";})
        .attr("transform", "scale(" + 1/zoom_scale + ")");
        
        nodeEnter.append("text")
          .attr("x", function(d) {
            if(d.parent) {
              // not the root
              if(d.children || d._children) {
                // not the leaves
                return d.parent.name != d.name ? "-30px" : -8;
              } else {
                //leaves
                return "30px";
              }
            } else {
              // root
              return "-30px";  
            }
          })
          .attr("y", function(d) {
            if(d.parent) {
              // not the root
              if(d.children || d._children) {
                // not the leaves
                return d.parent.name != d.name ? "0px" : ".35em";
              } else {
                //leaves
                return "0px";
              }
            } else {
              // root
              return "0px";  
            }
          })
          .attr("text-anchor", function(d) { return d.children || d._children ? "end" : "start"; })
          .attr("class", "label-image-text")
          .text(function(d) {return(d.name);})
          .attr("transform", "scale(" + 1/zoom_scale + ")");
          
        imageEnter.on("mouseover", function(selected){
          svg.selectAll("g.node").sort(function(a, b) {
            if (a.id === selected.id) {
              return 1;
            } else {
              if (b.id === selected.id) {
                return -1;
              } else {
                return 0;
              }
            }
          });
          
          svg.selectAll("g.node")
            .filter(function(a) {
              return(a.id == selected.id);
            })
            .selectAll("text.label-image-text")
            .style("display", "block");
          
        });
        
        imageEnter.on("mouseout", function(){
          svg.selectAll("text.label-image-text")
            .style("display", "none");
        });
    } else {
      nodeEnter.append("text")
        .attr("x", function(d) { return d.children || d._children ? -8 : 8; })
        .attr("dy", ".35em")
        .attr("text-anchor", function(d) { return d.children || d._children ? "end" : "start"; })
        .text(function(d) { 
          
          if(d.parent) {
            // not the root
            if(d.children || d._children) {
              // not the leaves
              return d.parent.name != d.name ? d.name : "";
            } else {
              //leaves
              return d.name;
            }
          } else {
            // root
            return d.name;
          }
        
           //return d.name;
        })
        .attr("class", "label-text")
        .style("fill-opacity", 1e-6)
        .attr("transform", "scale(" + 1/zoom_scale + ")");
    }

    // Transition nodes to their new position.
    var nodeUpdate = node.transition()
        .duration(duration)
        .attr("transform", function(d) { return "translate(" + x_map(d.height) + "," + d.x + ")"});
    
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
  
  /*
  function zoomed(d) {
    var x_val = slide_x.invert(d3.event.x);
    var right, left;
    
    
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
  */
  
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
    console.log(path_vec);
    
    // expand first level
    expand(dat);

    //recursively expand the correct children
    inner_nav(path_vec, dat);
    
    return(dat);
    
  }
  
  // inner function for nav_path to make recursion happen
  function inner_nav(path, data) {
    if (path.length > 1) {
      expand(data.children[+path[0]]);
      inner_nav(path.slice(1), data.children[+path[0]]);
    }
  }
  
  // zoom function
  function zoom() {
    zoom_scale = d3.event.scale;
    
    svg.attr("transform", "translate(" + d3.event.translate + ")" + " scale(" + zoom_scale + ")");
    
    // zoom the labels
    svg.selectAll("g.node>text,.node>circle,.node>image")
        .attr("transform", "scale(" + 1/zoom_scale + ")");
        
  }
  
}

// bring to front on hover
d3.selection.prototype.moveToFront = function() {
  return this.each(function(){
    this.parentNode.appendChild(this);
  });
};





var outputBinding = new Shiny.OutputBinding();
$.extend(outputBinding, {
  find: function(scope) {
    return $(scope).find('.select_custom');
  },
  renderValue: function(el, data) {
    select_wrapper(el, data);
  }});
Shiny.outputBindings.register(outputBinding);

var label;

var inputBinding = new Shiny.InputBinding();
$.extend(inputBinding, {
  find: function(scope) {
    return $(scope).find('.select_custom');
  },
  getValue: function(el) {
    return label;
  },
  subscribe: function(el, callback) {
    $(el).on("change.inputBinding", function(e) {
      callback();
    });
  },
});
Shiny.inputBindings.register(inputBinding);

function select_wrapper(el, data) {
  var selected_tab = data.tab;

  d3.select(el).select("select").remove();
  d3.select(el).select(".selectize-control").remove();
  
  // hide link buttons
  var links = d3.select(".link_buttons");
  links.style("visibility", "hidden");
  
    
  if(selected_tab == "Visualization") { // only have the search if on the viz tab
    
    //bring back link buttons
    links.style("visibility", "visible");
    
    var label_select = d3.select(el)
      .append("select")
        .attr("id", "select-label")
        .attr("class", "label");
    
    var first_elm = [['Search for first occurence of label:', []]];
    
    var labels = label_select.selectAll("option")
      .data(first_elm.concat(Object.entries(data.paths)));
      
    labels.enter().append("option")
      .attr("value", function(d){ return d[1]; })
      .text(function(d){ return d[0]; });
        
    var $select = $('#select-label').selectize({
  					create: false,
  					sortField: {
  						field: 'text',
  						direction: 'asc'
  					},
  					dropdownParent: 'body',
  					onChange: function(value) {
                 label = value;
            },
            allowEmptyOption: true
  				});
  	
  	var selectizeControl = $select[0].selectize;
	
    $("ul.nav.nav-tabs").append($(el));
  }  
  

}

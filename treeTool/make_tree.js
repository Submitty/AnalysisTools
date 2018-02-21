
function prune(nodes){
	for(var i=0;i<nodes.length;i++){

		if(nodes[i].data.type!="node"){     
			//console.log(nodes[i].parent); 
			if(nodes[i].parent.children.length==1){
				nodes[i].parent.children=null;
			}
			if(nodes[i].parent.data.tags.indexOf("function")!=-1){
				nodes[i].parent.data.tags.push(nodes[i].data.data);
			}

			nodes.splice(i,1);
			i--;

		}
	}
	return nodes;
}


function makeTree(nodes){



	var leaves=0;
	for(var i=0;i<nodes.descendants().length;i++){
		if(nodes.descendants()[i].height==0)
			leaves++;
	}


	var margin = {top:20, right:90, bottom:40, left:90},
			width=nodes.height*200-margin.left-margin.right,
			height=leaves*50-margin.top-margin.bottom;

	var treeMap = d3.tree()
			.size([height,width]);
	nodes=treeMap(nodes);

	//var output="";
	var svg=d3n.createSVG(width + margin.left + margin.right,height+margin.top+margin.bottom).attr("version","1.1"),
			g = svg.append("g")
			.attr("transform",
					"translate(" + margin.left + "," + margin.top + ")");

	nodes=prune(nodes.descendants());
	//console.log(output);
	var link = g.selectAll(".link")
			.data( nodes.slice(1))
			.enter().append("path")
			.attr("class", "link")
			.attr("d", function(d) {
				return "M" + d.y + "," + d.x
						+ "C" + (d.y + d.parent.y) / 2 + "," + d.x
						+ " " + (d.y + d.parent.y) / 2 + "," + d.parent.x
						+ " " + d.parent.y + "," + d.parent.x;
			})
			.style('stroke',function(d){
				for(var i=0;i<d.data.tags.length;i++){
					if(d.data.tags[i].startsWith("#"))
						return d.data.tags[i];
				}
				return "#2b79a5";
			})
			.style('stroke-opacity',"0.2");
	var node = g.selectAll(".node")
			.data(nodes)
			.enter().append("g")
			.attr("class", function(d) { 
				return "node" + 
						(d.children ? " node--internal" : " node--leaf"); })
			.attr("transform", function(d) { 
				return "translate(" + d.y + "," + d.x + ")"; });

	node.append("circle")
	.attr("r", 10)
	.style('fill',function(d){
		for(var i=0;i<d.data.tags.length;i++){
			if(d.data.tags[i].startsWith("#"))
				return d.data.tags[i];
		}
		return "#2b79a5";
	});

	// adds the text to the node
	node.append("text")
	.attr("dy", ".35em")
	.attr("x", function(d) { return d.children ? -13 : 13; })
	.style("text-anchor", function(d) { 
		return d.children ? "end" : "start"; })
	.text(function(d) { 
		var str="";
		if(d.data.tags!=undefined){
			str=d.data.tags[0];

			for(var i=1;i<d.data.tags.length;++i){
				if(!d.data.tags[i].startsWith("#")){
					str+=", "+d.data.tags[i];
				}
			}
		}

		return str;
	});
	console.log(d3n.svgString());
}


if(process.argv.length!=3){
	throw error;
}

const D3Node = require('d3-node');
const d3n = new D3Node({styles:'.node circle {fill: #2b79a5;}.node text {font: 12px sans-serif;}.link {fill: none;stroke: #e0e0e0;stroke-width: 2px;}'});
var d3=d3n.d3;
//console.log(process.argv.length);

var treeData=require(process.argv[2]);

/*d3.json(data,function(error,data){

	if(error){
	   	console.log("Error");
	   	throw error;
	} 
	console.log("processed json");
});*/
var nodes = d3.hierarchy(treeData, function(d) {
	return d.children;
});



//console.log(nodes);
makeTree(nodes);


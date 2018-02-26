var jq = require("jqgram").jqgram;
var fs = require("fs");
var str;
var root1;
var root2;
fs.readFile("outIntersect.txt", "utf8", function(err, data){
		if(err) throw err;
		str = data;	
		root1 = JSON.parse(str);
		readSecondFile();		
});

function readSecondFile(){
	fs.readFile("outUnion.txt", "utf8", function(err, data){
		if(err) throw err;
		str = data;
		root2 = JSON.parse(str);
		distance();
	});
}

function distance(){

jq.distance({
root: root1,
lfn: function(node){ return node.thelabel; },
cfn: function(node){ return node.thekids; }
},{
root: root2,
lfn: function(node){ return node.name; },
cfn: function(node){ return node.kiddos; }
},{ p:2, q:3, depth:10 },
function(result) {
console.log(result.distance);
});

}


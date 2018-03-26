// web scraping
//var request = require('request');
//var cheerio = require('cheerio');

// launch web server
var http = require('http');
var url = require("url");

const searchUrl = 'https://www.google.be/search?tbm=isch&source=hp&q=';

var localIP = "127.0.0.1"; // 127.0.0.1 is used when
var port = 8080;

var server = http.createServer(function (req, res) {
    console.log("We've got a request on " + req.url);

    var urlObj = url.parse(req.url, true);

	var defaultName = "test";
    if (urlObj['query']['image'] != undefined) {
        defaultName = urlObj['query']['image'];
    }
    
	res.setHeader('Access-Control-Allow-Origin', '*');
    res.setHeader('Content-Type', 'application/json');
	var json = JSON.stringify({ "data": 
		{ "image_url": "img/tomato.jpg"
		, "images": [ "img/tomato.jpg"
			        , "img/rice.png"
					, "img/water.jpg" ]}});
	res.end(json);

	var fullSearchUrl = searchUrl + defaultName;
	//var cbDecoder = R.pipe(crawlHtml, createJsonResponse);
	// request(fullSearchUrl, function(err, resp, body){

	//   // crawl HTML and find images
	//   console.log('searching for: ', fullSearchUrl);
	//   $ = cheerio.load(body);
	//   links = $('img'); //use your CSS selector here
	// 	console.log('links: ', links.length);

	//   // return JSON
	//   // prepare HTTP response header
	//   res.writeHead(200, {'Content-Type': 'text/html'});
	//   res.write('<html>\n<body>\n');
	//   res.write('<h1>Hello ' + defaultName + '<h1>\n');
	//   res.write('<form method="GET">\n' +
	//   		'<input type="text" placeholder="type a name" name="image">\n' +
	//   		'<input type="submit" value="submit new name">\n' +
	//   	'</form>\n');

	//   // HTTP response body
	//   const R = require("ramda");

	//   R.pipe( R.take(5)
	// 	    , R.forEach(i => res.write(htmlImage(i.attribs.src)))
	// 		)(links);
	//   
	//   res.write('</body>\n</html>');
	//   // HTTP response finished
	//   res.end();
	// });
});

var htmlImage = function(src) {
	var div = html => '<div style="float: left">' + html + '</div>'
	   ,img = src  => '<img src="' + src + '">'
	   ;
	return div(img(src))
}
server.listen(port, localIP);

// print message to terminal that server is running
console.log('Server running at http://'+ localIP +':'+ port +'/');

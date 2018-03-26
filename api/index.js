const startupTime = Date.now();

const request = require('request')
     ,cheerio = require('cheerio')
	 ,R       = require("ramda")
     ,http = require('http')
     ,url  = require("url");

const searchUrl = 'https://www.google.be/search?tbm=isch&source=hp&q='
	 ,localIP   = "127.0.0.1" 
	 ,port      = 8080;


//
// Functions
// ----------------------------------------

// crawlHtml : String -> Array Urls
var crawlHtml = html => {
	const $     = cheerio.load(html)
	     ,links = $('img');
	return R.pipe( R.take(5)
				 , obj => R.map(prop => R.prop(prop, obj)
					           ,['0', '1', '2', '3', '4'])
				 , R.map(img => img.attribs.src)
				 //, a => { console.log(a); return a }
				 )(links);
	}

// createResponse : Response -> JSON -> void
var createResponse = R.curry((resp, json) => {
	resp.setHeader('Access-Control-Allow-Origin', '*');
    resp.setHeader('Content-Type', 'application/json');
	resp.end(json);
});


// Application
// ----------------------------------------

var requestListener = function (req, res) {
    console.log("We've got a request on " + req.url);

    var urlObj = url.parse(req.url, true);

	var defaultName = "test";
    if (urlObj['query']['image'] != undefined) {
        defaultName = urlObj['query']['image'];
    }

	var fullSearchUrl = searchUrl + defaultName,
		sendJson = createResponse(res);

	var cbDecoder = R.pipe
	    ( (err, resp, html) => html	
		, crawlHtml
		, xs => JSON.stringify({'data': { 'images': xs }})
		, sendJson)
	request(fullSearchUrl, cbDecoder);
}

// start server
http.createServer(requestListener).listen(port, localIP);

// print message to terminal that server is running
console.log('Server running at http://'+ localIP +':'+ port +'/' +
			'\nStartup time: ' + (Date.now() - startupTime));

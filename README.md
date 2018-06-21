Nāga: Erlang Web Framework
==========================

work in progress ;-)

# Overview

Naga framework is a web development framework written in erlang which implements the server-side MVC pattern. [Naga](http://github.com/naga-framework/naga) is build on top of [N2O](http://synrc.com/) framework, many of  its components and concepts are from [ChicagoBoss](http://chicagoboss.org).

Naga provides the best of both framework - [ChicagoBoss](http://chicagoboss.org) and  [N2O](http://synrc.com/), for implementing realtime features in modern web HTML5 application.


Features
--------
* some CB & N2O ...
* live reload ala phoenix [(see)](https://github.com/naga-framework/naga/blob/9f4b6a895f107cab717ae7e3cb386545879bb250/src/naga_load.erl#L13)
* Desktop application erlang/electron-app 


Naga/Electron Desktop App
-------------------------
- bundle your electron-app with erlang runtime (or not), and your naga-app (mad bundle <appname>)

sys.config
```erlang
[{n2o, [...
        ,{protocols,[ naga_electron, %ws 
                      naga_load,     %live reload
                      ...
                    ]}
```

erlang: routes file
```erlang
,{ "/electron"       ,[{app, <appname>},{ctrl, electron} ,{act, index}], []}
 
```
erlang: naga controller
```erlang 
-module(electron).
-compile(export_all).

index(_,_,_) -> 404.
event({electron,init}) -> wf:wire("console.log('Hello World!!');"). 

```

- electron:main process you will need bert.js, utf8.js, client.js and the code below to establish a ws connection to your erlang app.
```javascript
$ws = { heart: true, interval: 4000,
        creator: function(url) { new WebSocket(url); },
        onheartbeat: function() { this.channel.send('PING'); } };

// N2O Reliable Connection

$conn = { onopen: nop, onmessage: nop, onclose: nop, onconnect: nop,
          send:  function(data)   { if (this.port.channel) this.port.channel.send(data); },
          close: function()       { if (this.port.channel) this.port.channel.close(); } };
ct = 0;
transports = [ $ws ];
heartbeat = null;
reconnectDelay = 1000;
maxReconnects = 100;

function nop() { }
function bullet(url) { $conn.url = url; return $conn; }
function xport() { return maxReconnects <= ct ? false : transports[ct++ % transports.length]; }
function reconnect() { setTimeout(function() { connect(); }, reconnectDelay); }
function next() { $conn.port = xport(); return $conn.port ? connect() : false; }
function connect() {
    $conn.port.channel = new WebSocket($conn.url);
    $conn.port.channel.onerror = function(e){};
    if (!$conn.port.channel) { setInterval(next(), 4000);} //retry until we establish cnx
    $conn.port.channel.onmessage = function(e) { $conn.onmessage(e); };
    $conn.port.channel.onopen = function() {
        if ($conn.port.heart) heartbeat = setInterval(function(){$conn.port.onheartbeat();}, $conn.port.interval);
        $conn.onopen();
        $conn.onconnect(); };
    $conn.port.channel.onclose = function() { $conn.onclose(); clearInterval(heartbeat); reconnect(); };
    return $conn; }
    
function NAGA_start(url) {
    ws = new bullet(url);ws.onerror = function(e){};
    ws.onmessage = function (evt) { // formatters loop
    for (var i=0;i<protos.length;i++) { p = protos[i]; if (p.on(evt, p.do).status == "ok") return; } };
    ws.onopen = function() { if (!active) { console.log('Connect'); ws.send(enc(tuple(atom('electron'),atom('init')))); active=true; } };
    ws.onclose = function() { active = false; console.log('Disconnect'); }; next(); }

function is(x,num,name) { return x.t==106?false:(x.v.length === num && x.v[0].v === name); }

/// N2O Protocols
var $io = {}; $io.on = function onio(r, cb) { if (is(r,3,'io')) {//console.log(utf8_dec(r.v[1].v));
    try { eval(utf8_dec(r.v[1].v)); if (typeof cb == 'function') cb(r); return { status: "ok" }; }
    catch (e) { console.log(e); return { status: '' }; } } else return { status: '' }; }

var protos = [$client];
```

```javascript
app.on('ready', function(){
   ....
   //start your erlang app as a child process using escript 
   ....
   //establish ws cnx with your erlang app
   NAGA_start("http://127.0.0.1:8001/ws/electron");
});
```


Demo
----

- [draw](https://github.com/naga-framework/draw) a realtime sharing drawing board with naga
- [cms](https://github.com/naga-framework/cms) boiler plate demo, work in progess


# Requirements


- [Erlang/OTP](http://www.erlang.org) version >= 18,  require maps

## Get Started

To create and build a naga application you need to install [mad](https://github.com/naga-framework/mad.git).


## Downloading

You can download mad directly from github:

    $ curl -O https://raw.githubusercontent.com/naga-framework/mad/naga_v2/mad

Then ensure it is executable

    $ chmod a+x mad

and drop it in your $PATH


## Build mad from source

```bash
    $ git clone https://github.com/naga-framework/mad.git
    $ cd mad
    $ make    
```

and drop it in your $PATH


## Create a naga app.

  mad integrate a small template engine.

```bash
    $ mad create name=hello tpl=basic port=9000 desc="hello app" vsn="0.0.1"  
```

```bash
>mad create name=hello tpl=basic port=9000 desc="my awesome hello app" vsn="0.0.1"
Name = "hello"
TplName = "basic"
Vars = [{port,"9000"},{tpl,"basic"},{name,"hello"}]
Writing hello/apps/hello/priv/hello.routes
Writing hello/apps/hello/rebar.config
Writing hello/apps/hello/src/hello.app.src
Writing hello/apps/hello/src/hello.erl
Writing hello/apps/rebar.config
Writing hello/rebar.config
Writing hello/sys.config
Writing hello/vm.args
Writing hello/apps/hello/src/controller/error.erl
Writing hello/apps/hello/src/controller/index.erl
Writing hello/apps/hello/src/view/error/404.html
Writing hello/apps/hello/src/view/index/index.html
Writing hello/apps/hello/src/view/layout/n2o.html
Writing hello/apps/hello/src/view/layout/page.html
OK
```  

## start to develop

```shell
>cd hello
>mad clean deps comp plan repl -name hello@127.0.0.1
```

# Donation



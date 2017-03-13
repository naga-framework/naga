Nāga: Erlang Web Framework
==========================

work in progress ;-)

#Overview

Naga framework is a web development framework written in erlang which implements the server-side MVC pattern. [Naga](http://github.com/naga-framework/naga) is build on top of [N2O](http://synrc.com/) framework, many of  its components and concepts are from [ChicagoBoss](http://chicagoboss.org).

Naga provides the best of both framework - [ChicagoBoss](http://chicagoboss.org) and  [N2O](http://synrc.com/), for implementing realtime features in modern web HTML5 application.


### Why an other web-framework

i love ChicagoBoss, i first started learning erlang by digging into it. by doing so, i started to contribute to it as i get more confidente with my erlang knowledge an the internal of CB.

i introduced a way to use websocket in ChicagoBoss. at that time it was good enought, but something was missing. i worked on [wamp](https://github.com/mihawk/wamp) to provide [wamp](http://wamp-proto.org/) WebSocket subprotocol.

few time later, i came accross n2o websocket server, claim that CB was slow. i checked myself and it was true :(, i tryed to solve different bottleneck in CB and to merge with n2o project.
finaly gave up because it reclaim to much change on CB code source.

late last year, i decided to build the way around, build on top of n2o project and get some properties of CB i found usefull.
the naga-project started. i try to keep it small as possible, less dependency, split it into different component mail,translation,fcgi. it was much easier, i end up with less code, which is great for maintening a project.


Features
--------

* Same features as [N2O](http://5ht.co/n2o.htm)
* Easy server–side templates
* Familiar Rails conventions
* Django Template, HTML DSL with nitro, SVG DSL with svg
* Fast Routing, compiled modules from a routes file.
* NO parametized controller module
* Naga app are more OTPish
* Multiple listener
* Small codebase
* Modulare components: [lang](https://github.com/naga-framework/naga_lang), [mail](https://github.com/naga-framework/naga_mail), [fcgi](https://github.com/naga-framework/naga_fcgi)
* Easy extensible (you can use any cowboy handler you wrote)

Exemples
--------

Ruby on Rails:

```ruby
def index
    case @request.method
    when "POST"
        # do something...
    when "GET"
        # do something...
    end
end  
```

Naga:

```erlang
 index(<<"POST">>, [], Ctx) ->
    # do something...
 index(<<"GET">>,  [], Ctx) ->
    # do something...
 
 event(Event) ->
    # do something
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

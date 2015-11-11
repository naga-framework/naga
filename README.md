Nāga: Erlang Web Framework (beta)
=================================

GOAL:
 - Keep it small, fast, less dependencies as much as possible, 
 - NO PMOD, 
 - Compiled module for routing
 - Bring ChicagoBoss flavour to n2o framework.
 

# Requirements

- [Erlang/OTP](http://www.erlang.org) version >= 17,  require maps

# Get Started

  to create and build a naga application you need to install [mad](https://github.com/naga-framework/mad.git).

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


## Create a Naga app:

  mad integrate a small template engine.

    $ mad create name=<app_name> tpl=<tpl_name> port=<9001>

```bash
mad create name=toto tpl=hello port=9000
Name = "toto"
TplName = "hello"
Vars = [{port,"9000"},{tpl,"hello"},{name,"toto"}]
Writing toto/apps/toto/rebar.config
Writing toto/apps/rebar.config
Writing toto/rebar.config
Writing toto/vm.args
Writing toto/sys.config
Writing toto/apps/toto/src/toto.app.src
Writing toto/apps/toto/src/toto.erl
Writing toto/apps/toto/src/controller/index.erl
Writing toto/apps/toto/src/controller/error.erl
Writing toto/apps/toto/src/toto_routes.erl
Writing toto/apps/toto/src/view/index/index.html
Writing toto/apps/toto/src/view/error/404.html
OK
```  
## start to develop

```shell
cd toto
mad deps comp plan repl
```

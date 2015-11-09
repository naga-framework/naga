#Nāga: Erlang Web Framework (beta)
=================================

##NAGA == M, [V, C] + websocket on steroid (n2o), the M is up to you (boss_db, kvs, ...)

GOAL:
 - keep it small, fast, less dependencies as much as possible, 
 - NO PMOD, 
 - compiled module base for routing
 - bring ChicagoBoss flavour to n2o framework instead of bringing n2o to CB. (less code) 
 

# Requirements

- [Erlang/OTP](http://www.erlang.org) version >= 17,  require maps

# Get Started

  to create and build a naga application you need to install [mad](https://github.com/naga-framework/mad.git).

## Downloading

You can download mad directly from github:

    $ curl -O https://raw.githubusercontent.com/naga-framework/mad/naga/mad

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

  mad integrate a small template engine for creating naga app

    $ mad naga create name=<app_name>

```bash
    $ mad naga create name=test
    Create Naga App Params: ["create","test"]
    Writing test/rebar.config
    Writing test/priv/test.config
    Writing test/src/test.app.src
    Writing test/src/test_app.erl
    Writing test/src/test_sup.erl
    Writing test/src/test.erl
    Writing test/src/controller/test_index_controller.erl
    Writing test/src/view/lib/tag_modules/test_custom_tags.erl
    Writing test/src/view/lib/filter_modules/test_custom_filters.erl
    Writing test/priv/test.routes
    Writing test/start.sh
    Writing test/src/view/index/index.html
    Writing test/src/view/lib/README
    Writing test/Makefile
    $ cd test
    $ make
```  

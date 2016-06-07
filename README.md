This project is a small example for how to set up a web-server with
[servant-server](http://haskell-servant.readthedocs.io/) that uses
[persistent](https://www.stackage.org/package/persistent) for saving data to a
database.

You can build and run the project with [stack](http://haskellstack.org/), e.g.:

``` bash
stack build
stack exec example-servant-persistent
```

Then you can query the server from a separate shell:

``` bash
curl -H 'Content-type: application/json' localhost:3000/user/add --data '{"name": "Alice", "age": 42}'
curl -H 'Content-type: application/json' localhost:3000/user/get/Alice
```

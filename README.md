# klashnikov

A dynamic HTTP load balancing reverse proxy. Supports HTTP 1.1 and HTTP 2.

## Usage

`klashnikov` pulls it's backend configuration from an `etcd` data store. It's recommended that `etcd3` be used alongside klashnikov, even though the GRPC interface isn't used - this is because `etcd3` has better history retention.

`klashnikov` requires a basic configuration file, written in `yaml`. It requires the address of the `etcd` instance to use for configuration, the range (directory) from which to source it's backend configuration, and the port upon which to listen. The file `klashnikov.yaml` provides an example of a basic configuration for a locally running instance of `etcd`, with backends described in the directory `/backends`, and with `klashnikov` listening on port `8080`.

`klashnikov` can then be run with `klashnikov config_file.yaml`. If experimenting using `stack` to build, you can run:

```
stack exec klashnikov-exe config_file.yaml
```

The values of the keys under the backend directory are expected to be in JSON objects, with a name field (text), address field (text), and a port (number). These can be PUT using curl. For instance, the following makes `klashnikov` reverse proxy to an `etcd` instance running locally with the default settings:

```
curl http://localhost:2379/v2/keys/backends/test -XPUT -d value="{\"name\":\"test\", \"address\":\"localhost\", \"port\":2379}"
```

## HTTP support

Currently `klashnikov` only allows one pool of backends, and doesn't distinguish different HTTP paths for requests. It just round robins all requests between the backends.

`klashnikov` can handle HTTP/2. It will reverse proxy HTTP/2 requests as HTTP/1.1 to the backends.
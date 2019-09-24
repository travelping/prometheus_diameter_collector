DIAMETER Prometheus.io collector
================================
[![Build Status][travis badge]][travis]

[Erlang Prometheus.io exporter](https://github.com/deadtrickster/prometheus.erl) for the
Erlang/OTP DIAMTER Application

Build
-----

    $ rebar3 compile

Metrics
-------

* `diameter_applications`<br />
Type: gauge
Labels: svc.<br />
Number of installed DIAMETER applications.

* `diameter_connections`<br />
Type: gauge
Labels: svc, peer, type, state, protocol.<br />
Number of connections to peers.

* `diameter_messages`<br />
Type: gauge
Labels: svc, peer, direction, type, msg, Result-Code.<br />
Number of requests.

## License

MIT

<!-- Badges -->
[travis]: https://travis-ci.com/travelping/prometheus_diameter_collector
[travis badge]: https://img.shields.io/travis/com/travelping/prometheus_diameter_collector/master.svg?style=flat-square

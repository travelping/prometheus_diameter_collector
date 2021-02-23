DIAMETER Prometheus.io collector
================================
[![Hex.pm Version][hexpm version]][hexpm]
[![Hex.pm Downloads][hexpm downloads]][hexpm]
[![Build Status][gh badge]][gh]
[![Erlang Versions][erlang version badge]][gh]

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

[Apache 2.0](LICENSE)

<!-- Badges -->
[hexpm]: https://hex.pm/packages/prometheus_diameter_collector
[hexpm version]: https://img.shields.io/hexpm/v/prometheus_diameter_collector.svg?style=flat
[hexpm downloads]: https://img.shields.io/hexpm/dt/prometheus_diameter_collector.svg?style=flat
[gh]: https://github.com/travelping/prometheus_diameter_collector/actions/workflows/main.yml
[gh badge]: https://img.shields.io/github/workflow/status/travelping/prometheus_diameter_collector/CI?style=flat-square
[erlang version badge]: https://img.shields.io/badge/erlang-R22.0%20to%2023.2-blue.svg?style=flat-square

https-everywhere-rules
======================

Unencrypted HTTP requests leave users vulnerable to surveillance, content filtering, and in some cases content tampering. While the number of websites making encrypted connections available are steadily increasing, only a fraction of those send visitors to their HTTPS version by default. While efforts to encourage broader and safer adoption of HTTPS-as-default are praiseworthy, we hold that automatic client-local TLS upgrades are a worthwhile privacy measure for the forseeable future. 

[HTTPS Everywhere](https://www.eff.org/https-everywhere) provides these facilities within the two most common web browsers. But there exist HTTP clients which aren’t browsers, and unsupported browsers. We wish to supply broader protections; discouraging adversaries from intercepting and manipulating traffic from a wider variety of clients. This library serves as the bedrock of that effort; it provides high-level programmatic access to the HTTPS Everywhere rules database, making it easier to specify "substitute this HTTP endpoint for a corresponding secure endpoint, if you know how."

It's very much a work-in-progress now, though you are very welcome to [contribute](#contributing) to that progress.

Usage
-----

The interface is straightforward:

```haskell
Î»: :m + Data.HTTPSEverywhere.Rules Network.URI
Î»: rules <- getRulesets
Î»: let Just it = parseURI "http://httpsnow.org"
Î»: rewriteURL rules it
Just https://httpsnow.org
```

There exist demonstration programs:

- [`redirect-proxy`](examples/RedirectProxy.hs) implements a naive redirecting HTTP proxy, writing 302s for any URI HTTPS Everywhere knows how to upgrade.

	```
	% nohup redirect-proxy &>/dev/null &
	[1] 674
	% curl -w '%{remote_ip} %{http_code} %{redirect_url}\n' -x localhost:3100 httpsnow.org
	127.0.0.1 302 https://httpsnow.org/
	```

- [`count-upgradeable`](examples/CountUpgradeable.hs) announces the fraction of URIs received on standard input that HE knows how to upgrade:

	```
	% find vendor/https-everywhere/src/chrome/content/rules -name '*.xml' | shuf -n100 > some_rules
	% < some_rules xargs grep default_off | wc -l
	20
	% < some_rules xargs grep -Pohr '(?<=test url=")[^"]+(?=")' | count-upgradeable
	(44,76)
	```

Contributing
------------

First of all, thank you.

This is all fairly immature as-is, but you're more than welcome to plod in. The best step you can take right now is to drop by the IRC channel [#https-everywhere-else](irc://irc.freenode.net/#https-everywhere-else) on Freenode: indicate what you want to be doing, and we'll work out how to make it happen; indicate any uncertainty, and we will try to clarify it.

Our current focus is on performance improvements. The current implementation relies on a linear search of the ruleset upon each rewrite attempt; this is compromisingly inefficient. We're going to modify the library to perform search on a supercompiled suffix tree, where rather than lookup being on the order of the number of rules, it will be on the order of the depth of the domain - the previous cost being incurred once at compile time.

Building and developing
--------

### nix
There is a working `default.nix`. You can run `nix-build`.
For development, you can run `nix-shell -A env`, and `runghc Setup.hs configure && runghc Setup.hs build`.

### stack
`stack build` should work.

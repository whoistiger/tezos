Development Changelog
'''''''''''''''''''''

**NB:** The changelog for releases can be found at: https://tezos.gitlab.io/CHANGES.html


This file lists the changes added to each version of tezos-node,
tezos-client, and the other Octez executables. The changes to the economic
protocol are documented in the ``docs/protocols/`` directory; in
particular in ``docs/protocols/alpha.rst``.

When you make a commit on master, you can add an item in one of the
following subsections (node, client, …) to document your commit or the
set of related commits. This will ensure that this change is not
forgotten in the final changelog, which can be found in ``docs/CHANGES.rst``.
By having your commits update this file you also make it easy to find the
commits which are related to your changes using ``git log -p -- CHANGES.rst``.
Relevant items are moved to ``docs/CHANGES.rst`` after each release.

Only describe changes which affect users (bug fixes and new features),
or which will affect users in the future (deprecated features),
not refactorings or tests. Changes to the documentation do not need to
be documented here either.

Node
----

- Added a store metric to expose the amount of data written.

- Added a block validator metric to expose the number of operation per
  pass for each new block validated

- Added a protocol specific metrics, head_cycle and head_consumed_gas.

- Added the ``jakartanet`` built-in network alias.
  You can now configure your node with ``--network jakartanet`` to run the
  Jakartanet test network.

Client
------

- Client allows to simulate failing operations with ``--simulation
  --force``, and report errors without specifying limits.

Accuser
-------

Signer
------

Proxy server
------------

Protocol Compiler And Environment
---------------------------------

Codec
-----

Docker Images
-------------

Miscellaneous
-------------

from typing import List
from client import client


class ClientRegression(client.Client):
    """Client to a Tezos node that intercepts output for regression testing.

    This subclass of Client can be associated with a `regtest` fixture
    as provided by the `pytest-regtest`, using the `set_regtest`
    method. When a `regtest` fixture is present, all output from the
    client that results from executing the `run` method is written to
    this regtest.
    """

    def __init__(self,
                 client_path: str,
                 admin_client_path: str,
                 host: str = '127.0.0.1',
                 base_dir: str = None,
                 rpc_port: int = 8732,
                 use_tls: int = False,
                 disable_disclaimer: bool = True):
        self.regtest = None
        super().__init__(client_path,
                         admin_client_path,
                         host,
                         base_dir,
                         rpc_port,
                         use_tls,
                         disable_disclaimer)

    def set_regtest(self, regtest):
        self.regtest = regtest

    def run(self,
            params: List[str],
            admin: bool = False,
            check: bool = True,
            trace: bool = False):
        output = super().run(params, admin, check, trace)
        if self.regtest is not None:
            self.regtest.write(output)
        return output

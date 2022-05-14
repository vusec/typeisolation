#!/usr/bin/env python3
# PYTHON_ARGCOMPLETE_OK
import sys
import os.path
curdir = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, os.path.join(curdir, 'infra'))

import infra
from infra.util import run, qjoin
from infra.targets.webservers import Nginx

class FakeInstance(infra.Instance):
    def __init__(self, name, preloader = None):
        self.name = name
        self.preloader = preloader

    def configure(self, ctx):
        pass

    def prepare_run(self, ctx):
        # Just before running the target, set LD_LIBRARY_PATH so that it can
        # find the dynamic library
        prevlibpath = os.getenv('LD_LIBRARY_PATH', '').split(':')
        libpath = os.path.join(curdir, 'autosetup.dir/install/' + self.name + '/lib')
        ctx.runenv.setdefault('LD_LIBRARY_PATH', prevlibpath).insert(0, libpath)
#        if self.preloader:
#            ctx.runenv.setdefault('LD_PRELOAD', self.preloader)

class FakeNginx(Nginx):
    def is_fetched(self, ctx):
        return True

    def fetch(self, ctx):
        pass

    def build(self, ctx, instance):
        pass

    def link(self, ctx, instance):
        pass

    def server_bin(self, ctx, instance):
        return os.path.join(curdir, 'autosetup.dir/targets/src/nginx-1.18.0/' + instance.name + '/objs/nginx')

if __name__ == '__main__':
    setup = infra.Setup(__file__)

    """
    setup.add_instance(FakeInstance('baseline-lto'))
    setup.add_instance(FakeInstance('typedallocator-typeisolation-stores'))
    setup.add_instance(FakeInstance('typesafestack-typedmalloc-inline'))
    setup.add_instance(FakeInstance('typesafestack-typedmalloc-inline-typeisolation-stores'))
    """

    setup.add_instance(FakeInstance('typedallocator-typeisolation-stores', '/tmp/preloadssl.so'))

    setup.add_target(FakeNginx('1.18.0'))

    setup.main()

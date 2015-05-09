#!/usr/bin/env python
# -*- coding: utf-8 -*-

import os
import subprocess


BASE_PATH = os.path.abspath(os.path.dirname(__file__))
CABAL_SANDBOX_FILE = 'cabal.sandbox.config'
CABAL_SANDBOX_HEADER = 'dist/build/autogen/cabal_macros.h'
DIST_TAGS = os.path.join(BASE_PATH, '.git/tags')
TMP_TAGS = os.path.join(BASE_PATH, 'tmp_tags')


class TagFile(object):

    def __init__(self):
        self.paths = []
        self.excepts = ['.git']

    @staticmethod
    def run(base):
        my = TagFile()
        if os.path.exists(TMP_TAGS):
            return
        my.delete_tags(base)
        my.search(base)
        my.merge()

    def delete_tags(self, path):
        cmd = 'find %s -name tags -type f -exec rm {} \;' % path
        subprocess.call(cmd, shell=True)

    def is_file(self, path):
        return os.path.isfile(path)

    def is_dir(self, path):
        return os.path.isdir(path)

    def full_path(self, base, file):
        return os.path.join(base, file)

    def add(self, path):
        self.paths.append(path)

    def merge(self):
        # pass
        tmp = ""
        for path in self.paths:
            with open(path) as f:
                tmp += f.read()
        cmd = 'cat %s | LANG=C sort > %s' % (TMP_TAGS, DIST_TAGS)
        with open(TMP_TAGS, 'w') as f:
            f.write(tmp)
        subprocess.call(cmd, shell=True)
        os.remove(TMP_TAGS)

    def has_header(self, path):
        return os.path.isfile(self.get_header(path))

    def get_header(self, path):
        return self.full_path(path, CABAL_SANDBOX_HEADER)

    def exec_hothasktags(self, path):
        tag = os.path.join(path, 'tags')
        cmd = (
            'find %s | egrep "\.hs$" | '
            'xargs hothasktags -c --noline -c --strip -c '
            '-I/root/workspace/headers '
            '-c --include=/root/workspace/headers/dummy.h '
            '%s '
            '> %s 2>/dev/null'
        )
        header = '-c --include=' + self.get_header(path) if self.has_header(path) else ''
        cmd = cmd % (path, header, tag)
        # print cmd
        subprocess.call(cmd, shell=True)
        self.add(tag)

    def search(self, base):
        dirs = []
        has_cabal = False
        for file in os.listdir(base):
            path = self.full_path(base, file)
            if self.is_file(path) and file == CABAL_SANDBOX_FILE:
                has_cabal = True
            elif self.is_dir(path) and file not in self.excepts:
                dirs.append(path)
        if has_cabal:
            self.exec_hothasktags(base)
            return False
        # cabalファイルがなければディレクトリを下に探す
        if dirs.count != 0:
            return self.search_more(dirs)
        else:
            # これ以下にはディレクトが存在しない
            return True

    def search_more(self, dirs):
        targets = []
        ret = True
        for dir in dirs:
            if self.search(dir) is False:
                ret = False
            else:
                targets.append(dir)
        if ret is False:
            for target in targets:
                self.exec_hothasktags(target)
        return ret


def main():
    TagFile.run(BASE_PATH)

if '__main__' == __name__:
    main()

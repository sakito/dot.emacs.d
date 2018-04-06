#!/usr/bin/env python
# -*- coding: utf-8 -*-
# Copyright (C) 2015 sakito <sakito@sakito.com>

from __future__ import print_function

import dircache
import os

from fabric.api import (
    task,
    local,
    lcd,
)


@task
def up():
    """
    更新
    """
    print('update')
    dir_list = dircache.listdir('.')
    for dir_name in dir_list:
        if os.path.isdir(dir_name):
            print('----------')
            print(dir_name)

            with lcd(dir_name):
                if os.path.exists('{}/.hg'.format(dir_name)):
                    local('hg pull; hg up')
                elif os.path.exists('{}/.git'.format(dir_name)):
                    local('git pull; git fetch')
                else:
                    local('pwd')


@task
def st():
    """
    確認
    """
    print('status')
    dir_list = dircache.listdir('.')
    for dir_name in dir_list:
        if os.path.isdir(dir_name):
            print('----------')
            print(dir_name)

            with lcd(dir_name):
                if os.path.exists('{}/.hg'.format(dir_name)):
                    local('hg st')
                elif os.path.exists('{}/.git'.format(dir_name)):
                    local('git incoming')
                else:
                    local('pwd')

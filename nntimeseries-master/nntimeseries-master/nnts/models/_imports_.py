# -*- coding: utf-8 -*-
"""
Imports file.
"""

import sys, os
sep = os.path.sep
sys.path.append(sep.join(os.path.abspath(__file__).split(sep)[:-3]))
#print(sys.path)
#for mod in ['utils', 'nnts', 'np', 'pd']:
#    print(mod + ': ', mod in globals())

import nnts
from nnts import *
from nnts.utils import *

#nnts.config.WDIR = path + '\\'
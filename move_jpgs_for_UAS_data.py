#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Sat May 16 18:37:08 2020

@author: adamdixon
"""

import os
import shutil
import glob
#set directory location
dir=r'E:\Dropbox\A_School\2020_GrassyMargins\2019_data\UAS_data'

y = [x[0] for x in os.walk(dir)]

for w in y:
    #set which directories wanted, in this case ones that were named NIR or RGB
    if w[-3:] == "NIR" or w[-3:] == "RGB":
        print(w)
        os.mkdir(os.path.join(w, "jpgs"))
        pattern = '*.JPG'
        for name in glob.glob(os.path.join(w,pattern)):
            shutil.move(name,os.path.join(w, "jpgs"))

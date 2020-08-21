#!/usr/bin/env python

from netCDF4 import Dataset
import numpy as np
import matplotlib.pyplot as plt
from mpl_toolkits.basemap import Basemap
from pylab import *

fig=plt.figure(figsize=(15.7,12.3))
ax =plt.subplot(111)

map = Basemap(projection="merc",llcrnrlat=-25.5,urcrnrlat=-19.5,\
	llcrnrlon=-53.0,urcrnrlon=-45.0,resolution='c')


map.readshapefile('./MunRM07','rmsp',drawbounds=True)

print len(map.rmsp)
print map.rmsp_info[0].keys()

for xy,info in zip(map.rmsp, map.rmsp_info):
	if info['AREA_KM2'] != 0:
		poly=Polygon(xy,facecolor="grey",alpha=0.4)
		plt.gca().add_patch(poly)
	else:
		poly=Polygon(xy,facecolor="white")
		plt.gca().add_patch(poly)

plt.show()
	

import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
from mpl_toolkits.basemap import Basemap
from matplotlib.path import Path
import matplotlib.patches as patches
from matplotlib.patches import Polygon
from netCDF4 import Dataset

# Loading wrfinputs
inp1 = Dataset('../02_data/wrfinputs/wrfinput_d01')
inp2 = Dataset('../02_data/wrfinputs/wrfinput_d02')
inp3 = Dataset('../02_data/wrfinputs/wrfinput_d03')

# Retrieving lat and lon of domains
xlat1 = inp1.variables['XLAT'][:]
xlon1 = inp1.variables['XLONG'][:]
xlat2 = inp2.variables['XLAT'][:]
xlon2 = inp2.variables['XLONG'][:]
xlat3 = inp3.variables['XLAT'][:]
xlon3 = inp3.variables['XLONG'][:]

inp1.close()
inp2.close()
inp3.close()

# coordinates of Pinheiros AQS and PAulista Avenue
pin_pau_lon = [-46.7020, -46.6727]
pin_pau_lat = [-23.5614, -23.5535]

# Beginning the plot
fig = plt.figure(figsize=(15, 8))
ax0 = plt.subplot2grid((2, 2), (0, 0))
ax1 = plt.subplot2grid((2, 2), (1, 0))
ax2 = plt.subplot2grid((2, 2), (0, 1))
ax3 = plt.subplot2grid((2, 2), (1, 1))

m0 = Basemap(projection='ortho', lon_0 = -60, lat_0=-15, ax=ax0)
m0.drawmapboundary(fill_color='#afeeee')
m0.fillcontinents(color='beige',lake_color='#afeeee')
m0.drawcoastlines()
m0.drawcountries()


m1 = Basemap(llcrnrlon=xlon1.min(), llcrnrlat=xlat1.min(),
            urcrnrlon=xlon1.max(), urcrnrlat=xlat1.max(),
            projection='merc', resolution='i', ax = ax1)
m1.drawcoastlines()
m1.drawstates()
m1.drawcountries()
m1.drawmapboundary(fill_color='#afeeee')
m1.fillcontinents(color='beige', lake_color='#afeeee', zorder=1)
m1.drawparallels(np.arange(-90., 90., 3), linewidth=0,
                labels=[1, 0, 0 ,0], fontsize=10)
m1.drawmeridians(np.arange(-180., 180., 4.5), linewidth=0,
                labels=[0, 0, 0, 1], fontsize=10)
m1.readshapefile('../02_data/rmsp_shp/MunRM07','MunRM07')
x1, y1 = m1(-40, -32.5)
ax1.text(x1, y1, 'D01', fontsize=20, weight = 'bold')

m2 = Basemap(llcrnrlon=xlon2.min(), llcrnrlat=xlat2.min(),
            urcrnrlon=xlon2.max(), urcrnrlat=xlat2.max(),
            projection='merc', resolution='h', ax=ax2)
m2.drawcoastlines()
m2.drawstates()
m2.drawcountries()
m2.drawmapboundary(fill_color='#afeeee')
m2.fillcontinents(color='beige', lake_color='#afeeee', zorder=1)
m2.drawparallels(np.arange(-90., 90., 1.0), linewidth=0,
                labels=[1, 0, 0 ,0], fontsize=10)
m2.drawmeridians(np.arange(-180., 180., 1.5), linewidth=0,
                labels=[0, 0, 0, 1], fontsize=10)
m2.readshapefile('../02_data/rmsp_shp/MunRM07','MunRM07')
x2, y2 = m2(-44.75, -26.5)
ax2.text(x2, y2, 'D02', fontsize=20, weight = 'bold')
for info, shape in zip(m2.MunRM07_info, m2.MunRM07):
    if info['SIGLA'] == 'SAO':
        xs, ys = zip(*shape)

m3 = Basemap(llcrnrlon=xlon3.min(), llcrnrlat=xlat3.min(),
            urcrnrlon=xlon3.max(), urcrnrlat=xlat3.max(),
            projection='merc', resolution='h', ax=ax3)
m3.drawcoastlines()
m3.drawstates()
m3.drawcountries()
m3.drawmapboundary(fill_color='#afeeee')
m3.fillcontinents(color='beige', lake_color='#afeeee', zorder=1)
m3.drawparallels(np.arange(-90., 90., 0.15), linewidth=0,
                labels=[1, 0, 0 ,0], fontsize=10)
m3.drawmeridians(np.arange(-180., 180., 0.25), linewidth=0,
                labels=[0, 0, 0, 1], fontsize=10)
m3.readshapefile('../02_data/rmsp_shp/MunRM07','MunRM07')
x3, y3 = m3(-46.37, -23.98)
xpin, ypin = m3(pin_pau_lon, pin_pau_lat)
m3.scatter(xpin, ypin, marker = 'o', color = "red", zorder=5, s=10)
ax3.text(x3, y3, 'D03', fontsize=20, weight = 'bold')
ax3.text(xpin[0], ypin[0], 'PIN', fontsize=8, ha ="right", va="top")
ax3.text(xpin[1], ypin[1], 'PAU', fontsize=9, ha="left", va="bottom")
for info, shape in zip(m3.MunRM07_info, m3.MunRM07):
    if info['SIGLA'] == 'SAO':
        xs, ys = zip(*shape)
        m3.plot(xs, ys, marker=None, color='black',
                linewidth=1.5)


lbx0, lby0 = m0(*m1(m1.xmin, m1.ymin, inverse= True))
ltx0, lty0 = m0(*m1(m1.xmin, m1.ymax, inverse= True))
rtx0, rty0 = m0(*m1(m1.xmax, m1.ymax, inverse= True))
rbx0, rby0 = m0(*m1(m1.xmax, m1.ymin, inverse= True))

verts1 = [
    (lbx0, lby0), # left, bottom
    (ltx0, lty0), # left, top
    (rtx0, rty0), # right, top
    (rbx0, rby0), # right, bottom
    (lbx0, lby0), # ignored
    ]

codes2 = [Path.MOVETO,
         Path.LINETO,
         Path.LINETO,
         Path.LINETO,
         Path.CLOSEPOLY,
         ]

path = Path(verts1, codes2)
patch = patches.PathPatch(path, facecolor='r', alpha = 0.4, lw=1)
ax0.add_patch(patch)


lbx1, lby1 = m1(*m2(m2.xmin, m2.ymin, inverse= True))
ltx1, lty1 = m1(*m2(m2.xmin, m2.ymax, inverse= True))
rtx1, rty1 = m1(*m2(m2.xmax, m2.ymax, inverse= True))
rbx1, rby1 = m1(*m2(m2.xmax, m2.ymin, inverse= True))

verts1 = [
    (lbx1, lby1), # left, bottom
    (ltx1, lty1), # left, top
    (rtx1, rty1), # right, top
    (rbx1, rby1), # right, bottom
    (lbx1, lby1), # ignored
    ]

codes2 = [Path.MOVETO,
         Path.LINETO,
         Path.LINETO,
         Path.LINETO,
         Path.CLOSEPOLY,
         ]

path = Path(verts1, codes2)
patch = patches.PathPatch(path, facecolor='r', alpha = 0.4, lw=1)
ax1.add_patch(patch)

lbx2, lby2 = m2(*m3(m3.xmin, m3.ymin, inverse= True))
ltx2, lty2 = m2(*m3(m3.xmin, m3.ymax, inverse= True))
rtx2, rty2 = m2(*m3(m3.xmax, m3.ymax, inverse= True))
rbx2, rby2 = m2(*m3(m3.xmax, m3.ymin, inverse= True))

verts2 = [
    (lbx2, lby2), # left, bottom
    (ltx2, lty2), # left, top
    (rtx2, rty2), # right, top
    (rbx2, rby2), # right, bottom
    (lbx2, lby2), # ignored
    ]

codes2 = [Path.MOVETO,
         Path.LINETO,
         Path.LINETO,
         Path.LINETO,
         Path.CLOSEPOLY,
         ]

path = Path(verts2, codes2)
patch = patches.PathPatch(path, facecolor='r', alpha = 0.4, lw=1)
ax2.add_patch(patch)
plt.subplots_adjust(wspace=-0.5, hspace=0.15)

plt.savefig('../03_output/paper_figs/Fig02_wrf_3_domains.svg', bbox_inches='tight', dpi=300)

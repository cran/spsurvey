\name{spsurvey-internal}
\alias{mdmpts}
\alias{mdmlin}
\alias{mdmarea}
\alias{cell.wt}
\alias{selectpts}
\alias{selectframe}
\alias{ftnt.inv.fcn}
\alias{vecprint}
\alias{shape2spList}
\alias{ranho}
\alias{readDbfFile}
\alias{readShapeFile}
\alias{readShapeFilePts}
\alias{getRecordShapeSizes}
\alias{writeShapeFilePoint}
\alias{writeShapeFilePolygon}
\alias{pointInPolygonObj}
\alias{numLevels}
\alias{constructAddr}
\alias{pickGridCells}
\alias{pointInPolygonFile}
\alias{linSample}
\alias{getRecordIDs}
\alias{getShapeBox}
\alias{linSampleIRS}

\alias{dframe.check}
\alias{input.check}
\alias{vecprint}
\alias{uniqueID}
\alias{catvar.prop}
\alias{catvar.size}
\alias{cdf.nresp}
\alias{cdf.prop}
\alias{cdf.total}
\alias{cdf.size.prop}
\alias{cdf.size.total}
\alias{cdfvar.prop}
\alias{cdfvar.total}
\alias{cdfvar.size.prop}
\alias{cdfvar.size.total}
\alias{simex}
\alias{dcdf.prop}
\alias{dcdf.total}
\alias{dcdf.size.prop}
\alias{dcdf.size.total}
\alias{dcdfvar.prop}
\alias{dcdfvar.total}
\alias{dcdfvar.size.prop}
\alias{dcdfvar.size.total}
\alias{isotonic}
\alias{sorted}
\alias{cdfvar.test}
\alias{total.var}
\alias{relrisk.var}
\alias{localmean.weight}
\alias{localmean.var}
\alias{localmean.cov}
\alias{localmean.df}
\alias{wnas}
\alias{input.format}
\alias{warnprnt}
\title{Internal spsurvey Functions}
\description{
  These functions are internal spsurvey functions.
}
\usage{
mdmpts(mdcaty, n.desired)
mdmlin(len, mdcaty, n.desired)
mdmarea(area, mdcaty, n.desired)
cell.wt(cel, xc, yc, dx, dy, pts)
selectpts(rdx, xc, yc, dx, dy, pts)
selectframe(rord, xc, yc, dx, dy, pts)
ftnt.inv.fcn(y, a=1)
vecprint(x, n.char=78)
shape2spList(shape, shp.type, ID)
ranho(adr, size)
readDbfFile(fileNamePrefix)
readShapeFile(fileNamePrefix)
readShapeFilePts(fileNamePrefix)
getRecordShapeSizes(fileNamePrefix)
writeShapeFilePoint(xVec, yVec, prjFileNameVec, dbfFieldNames, dbfFields,
   filePrefix)
writeShapeFilePolygon(shapeTypeVal, fileLengthVal, contentLenVec, nPartsVec,
   nPointsVec, partsVec, xVec, yVec, prjFileNameVec, dbfFieldNames, dbfFields,
   filePrefix)
pointInPolygonObj(ptXVec, ptYVec, polyXVec, polyYVec)
numLevels(fileNamePrefix, nsmpVec, shiftGridVec,
   startLevVec, maxLevVec, dsgnmdIDVec, dsgnmdVec)
constructAddr(xcVec, ycVec, dxVec, dyVec, nlevVec)
pickGridCells(samplesize, idxVec)
pointInPolygonFile(fileNamePrefix, xcsVec, ycsVec, dsgnmdIDVec, dsgnmdVec)
linSample(fileNamePrefix, xcVec, ycVec, dxVec, dyVec, dsgnmdIDVec, dsgnmdVec)
getRecordIDs(areaCumSumVec, sampPosVec, dsgnIDVec)
getShapeBox(fileNamePrefix, dsgnIDVec)
linSampleIRS(fileNamePrefix, lenCumSumVec, sampPosVec, dsgnIDVec, dsgnLenVec,
   dsgnMdmVec)

dframe.check(sites, design, subpop, data.cat, data.cont,
   design.names)
input.check(nresp, wgt, sigma, var.sigma, xcoord, ycoord,
  stratum.ind, stratum, stratum.levels, nstrata, cluster.ind,
  cluster, cluster.levels, ncluster, N.cluster, wgt1, xcoord1,
  ycoord1, popsize, stage1size, pcfactor.ind, support,
  swgt.ind, swgt, swgt1, unitsize, vartype, conf, cdfval=NULL,
  pctval=NULL, subpop=NULL)
vecprint(x, n.char=78)
uniqueID(siteID)
catvar.prop(z, wgt, x, y, prop, stratum.ind, stratum.level,
  cluster.ind, cluster, N.cluster, wgt1, x1, y1, popsize,
  pcfactor.ind, stage1size, support, vartype, warn.ind,
  warn.df, warn.vec)
catvar.size(z, wgt, x, y, size, stratum.ind, stratum.level,
  cluster.ind, cluster, N.cluster, wgt1, x1, y1, popsize,
  pcfactor.ind, stage1size, support, vartype, warn.ind,
  warn.df, warn.vec)
cdf.nresp(z, val)
cdf.prop(z, wgt, val, cluster.ind, cluster, wgt1)
cdf.total(z, wgt, val, cluster.ind, cluster, wgt1, popsize)
cdf.size.prop(z, wgt, swgt, val, cluster.ind, cluster,
  wgt1, swgt1)
cdf.size.total(z, wgt, swgt, val, cluster.ind, cluster, 
  wgt1, swgt1, unitsize)
cdfvar.prop(z, wgt, x, y, val, cdfest, stratum.ind,
  stratum.level, cluster.ind, cluster, N.cluster, wgt1, x1,
  y1, popsize, pcfactor.ind, stage1size, support, vartype,
  warn.ind, warn.df, warn.vec)
cdfvar.total(z, wgt, x, y, val, cdfest, stratum.ind,
  stratum.level, cluster.ind, cluster, N.cluster, wgt1, x1,
  y1, popsize, pcfactor.ind, stage1size, support, vartype,
  warn.ind, warn.df, warn.vec)
cdfvar.size.prop(z, wgt, x, y, val, cdfest, stratum.ind,
  stratum.level, cluster.ind, cluster, N.cluster, wgt1, x1,
  y1, popsize, pcfactor.ind, stage1size, support, swgt, swgt1,
  vartype, warn.ind, warn.df, warn.vec)
cdfvar.size.total(z, wgt, x, y, val, cdfest, stratum.ind,
  stratum.level, cluster.ind, cluster, N.cluster, wgt1, x1,
  y1, popsize, pcfactor.ind, stage1size, support, swgt, swgt1,
  unitsize, vartype, warn.ind, warn.df, warn.vec)
cdfvar.test (sampl, bounds, vartype)
simex(z, val, sigma, var.sigma, cluster.ind, cluster)
dcdf.prop(g, wgt, cluster.ind, cluster, wgt1)
dcdf.total(g, wgt, cluster.ind, cluster, wgt1, popsize)
dcdf.size.prop(g, wgt, swgt, cluster.ind, cluster, wgt1,
  swgt1)
dcdf.size.total(g, wgt, swgt, cluster.ind, cluster, wgt1,
  swgt1, unitsize)
dcdfvar.prop(g, dg, var.sigma, wgt, x, y, cdfest,
   stratum.ind, stratum.level, cluster.ind, cluster,
   N.cluster, wgt1, x1, y1, popsize, pcfactor.ind, stage1size,
   support, vartype, warn.ind, warn.df, warn.vec)
dcdfvar.total(g, dg, var.sigma, wgt, x, y, cdfest,
   stratum.ind, stratum.level, cluster.ind, cluster,
   N.cluster, wgt1, x1, y1, popsize, pcfactor.ind, stage1size,
   support, vartype, warn.ind, warn.df, warn.vec)
dcdfvar.size.prop(g, dg, var.sigma, wgt, x, y, cdfest,
   stratum.ind, stratum.level, cluster.ind, cluster,
   N.cluster, wgt1, x1, y1, popsize, pcfactor.ind, stage1size,
   support, swgt, swgt1, vartype, warn.ind, warn.df, warn.vec)
dcdfvar.size.total(g, dg, var.sigma, wgt, x, y, cdfest,
   stratum.ind, stratum.level, cluster.ind, cluster,
   N.cluster, wgt1, x1, y1, popsize, pcfactor.ind, stage1size,
   support, swgt, swgt1, unitsize, vartype, warn.ind, warn.df,
   warn.vec)
isotonic(y, minval, maxval)
sorted(x)
total.var (z, wgt, x, y, mean.est, var.est, sd.est,
  stratum.ind, stratum.level, cluster.ind, cluster, N.cluster,
  wgt1, x1, y1, popsize, pcfactor.ind, stage1size, support,
  vartype, warn.ind, warn.df, warn.vec)
relrisk.var(response, stressor, response.levels,
   stressor.levels, wgt, x, y, stratum.ind, stratum.level,
   cluster.ind, cluster, N.cluster, wgt1, x1, y1, popsize,
   pcfactor.ind, stage1size, support, vartype, warn.ind,
   warn.df, warn.vec)
localmean.weight(x, y, prb, nbh)
localmean.var(z, weight.lst)
localmean.cov(zmat, weight.lst)
localmean.df(weight.lst)
wnas(data)
input.format(x, n.digits, miss="NA")
warnprnt(warn.df, m=1:nrow(warn.df))
}
\details{
  These functions are not to be called by the user.
}
\keyword{internal}

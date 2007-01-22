luck.ash              package:spsurvey              R Documentation

_E_x_a_m_p_l_e _P_o_l_y_g_o_n_s _D_a_t_a_s_e_t

_D_e_s_c_r_i_p_t_i_o_n:

     This dataset is a list composed of two element, each of which is a
     matrix containing the x-coordinates and y-coordinates for a
     polyline.

_U_s_a_g_e:

     data(luck.ash)

_F_o_r_m_a_t:

     A list containing two elements:

     '_1' a matrix containing two variables named xcoord and ycoord,
          which are the x-coordinates and y-coordinates for the first
          polyline.

     '_2' a matrix containing two variables named xcoord and ycoord,
          which are the x-coordinates and y-coordinates for the second
          polyline.

     '_3' a matrix containing two variables named xcoord and ycoord,
          which are the x-coordinates and y-coordinates for the third
          polyline.

     '_4' a matrix containing two variables named xcoord and ycoord,
          which are the x-coordinates and y-coordinates for the fourth
          polyline.

     '_5' a matrix containing two variables named xcoord and ycoord,
          which are the x-coordinates and y-coordinates for the fifth
          polyline.

_S_o_u_r_c_e:

     This dataset is a subset of all perennial and intermittent streams
     and rivers in  the Luckiamute Watershed Council basin in Oregon. 
     Watershed boundaries were  defined by the Luckiamute Watershed
     Council.  The GIS stream network coverage  was obtained from the
     Pacific Northwest (PNW) portion of  the U.S. EPA reach  file
     system (RF3).

_R_e_f_e_r_e_n_c_e_s:

     Horn, C.R. and Grayman, W.M. (1993). Water-quality modeling with
     EPA reach file  system.  _Journal of Water Resources Planning and
     Management_, *119*, 262-274.

_E_x_a_m_p_l_e_s:

     # This example converts the dataset to an sp package object
     data(luck.ash)
     n <- length(luck.ash)
     nparts <- rep(1, n)
     IDs <- as.character(1:n)
     shapes <- vector(mode="list", length=n)
     for(i in 1:n) {
        shapes[[i]] <- list(Pstart=0, verts=luck.ash[[i]], 
           nVerts=nrow(luck.ash[[i]]), nParts=nparts[i])
     }
     PolylinesList <- vector(mode="list", length=n)
     for(i in 1:n) {
       PolylinesList[[i]] <- shape2spList(shape=shapes[[i]], shp.type="arc",
          ID=IDs[i])
     }
     att.data <- data.frame(id=1:n, length=rep(NA, n))
     rownames(att.data) <- IDs
     sp.obj <- SpatialLinesDataFrame(sl=SpatialLines(LinesList=PolylinesList),
        data=att.data)
     # To convert the sp package object to a shapefile use the following code: 
     # sp2shape(sp.obj, "luck.ash")


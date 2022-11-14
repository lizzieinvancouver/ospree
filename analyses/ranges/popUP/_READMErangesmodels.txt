Started 22 April 2021
Updated 14 Nov 2022

- joint_climvar_3param_osp_ncpPhoto.stan -- same as joint_climvar_3param_osp.stan but with a NCP on the photoperiod beta

- rangeleadin_osp.R runs the current models; specifically these two lines:
	gddlf_jnt.eu= stan('popUP/stan/joint_climvar_3param_osp_ncpPhotoForce.stan', data = bb.gddlf.eu, iter = 5000, warmup=4000)
	gddlf_jnt.nam = stan('popUP/stan/joint_climvar_3param_osp_ncpPhotoForce.stan', data =bb.gddlf.nam, iter = 4000, warmup=3000) # Purring away now!

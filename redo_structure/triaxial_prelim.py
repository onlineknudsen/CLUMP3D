# (* ::Package:: *)
#
#
#
#
# BeginPackage["TriaxialPreliminariesv02`", {"StatisticalEstimatorsv01`","Posteriorv03`"}];
#
#
# Hubble\[CapitalLambda]::usage = "...";
# Dist\[CapitalLambda]::usage = "...";
# Dist\[CapitalLambda]Flat::usage = "...";
# Dist\[CapitalLambda]Close::usage = "...";
# Dist\[CapitalLambda]Open::usage = "...";
#
# D1\[CapitalLambda]::usage = "...";
# D1\[CapitalLambda]Approx::usage = "...";
#
# Hubble\[Omega]::usage = "...";
# Hubble\[Omega]Flat::usage = "...";
# Dist\[Omega]::usage = "...";
# Dist\[Omega]Flat::usage = "...";
#
#
# fSZ::usage = "...";
# gSZ::usage = "...";
#
#
# f\[Epsilon]Conv::usage="";
# f\[Epsilon]ConvDeg::usage="";
#
#
# SubMKS={s->1,day->24*60*60,year->365.25*24*60*60,g->10^-3,
# gr->10^-3,
# cm->10^-2,km->10^3,pc->3.08567802*10^16,kpc->10^3 pc,Mpc->10^6 pc,Gpc->10^9 pc,AU->1.49597870696*10^11 (* m *),
# G->6.673`*^-11 (*(Meter^2 Newton)/Kilogram^2*),c->299792458 (*Meter/Second*) ,kB->1.3806503*10^-23(* m^2 kg s^-2 K^-1*),TCMB->2.728(*K*),
# me->9.1093897 *10^-31 ,mu->1.660538921*10^-27(*unit\[AGrave] di massa atomica, kg*),mp(*proton mass*)->1.672621777*10^-27,MSun->1.9891030 *10^30(*kg*),
# \[Sigma]T->6.65246*10^-29(* m^2 *),hPlanckConstant->6.6260755*10^-34(*kg m^2 s^-1*),
# eV->1.60217653*10^-19(*J=kg m^2 s^-2*),keV->10^3 eV,erg->10^-7(*Joule*),
# Jy->10^-26};
SubMKS = {
    's':1,
    'day':24*60*60,
    'year':365.25*24*60*60,
    'g':10**-3,
    'gr':10**-3,
    'cm':10**-2,
    'km':10**3,
    'pc':3.08567802*(10**16),
    'AU':1.49597870696*(10**11), # m
    'G':6.673*(10**-11), # (Meter^2 Newton)/Kilogram^2
    'c':299792458, # Meter/Second
    'kB':1.3806503*(10**-23), # m^2 kg s^-2 K^-1
    'TCMB':2.728, # K
    'me':9.1093897*(10**-31),
    'mu':1.660538921*(10**-27), # unit\[AGrave] di massa atomica, kg
    'mp':1.672621777*(10**-27), # proton mass
    'MSun':1.9891030*(10**30), # kg
    'SigmaT':6.65246*(10**-29), # m^2
    'hPlanckConstant':6.6260755*(10**-34), # kg m^2 s^-1
    'eV':1.60217653*(10**-19), # J=kg m^2 s^-2
    'erg':10**-7, # Joule
    'Jy':10**-26
}
SubMKS['kpc'] = 10**3 * SubMKS['pc']
SubMKS['Mpc'] = 10**6 * SubMKS['pc']
SubMKS['Gpc'] = 10**9 * SubMKS['pc']
SubMKS['keV'] = 10**3 * SubMKS['eV']

assert len(SubMKS) == 26 # confirmation of length from Mathematica notebook

#
#
# DegToRad=\[Pi]/180;
# RadToDeg=DegToRad^-1;
# RadToArcMin=(180*60)/\[Pi];
# RadToArcSec=(180*60*60)/\[Pi];
# ArcSecToRad=RadToArcSec^-1;
# ArcSecToArcMin=1/60;
# ArcMinToArcSec=ArcSecToArcMin^-1;
# ArcMinToRad=\[Pi]/(180*60);
# DegToArcSec=DegToRad*RadToArcSec;
# mTocm=10^2;
# eVtoJoule= 1.60217733*10^-19;
# JouleToKeV=1/(eVtoJoule*10^3);
# keVToK=(1keV)/kB/.SubMKS/.SubMKS;
#
#
# H0TohSub={H0->100h km/(10^6 pc)}/.SubMKS//N;
# CritSub={\[CapitalSigma]Cr->c^2/(4\[Pi] G) Ds/(Dd Dds),\[Rho]Cr->(3H^2)/(8\[Pi] G)};
# \[CapitalOmega]Mz=(\[CapitalOmega]M0 (1+z)^3)/(\[CapitalOmega]M0 (1+z)^3+(1-\[CapitalOmega]M0));
#
#
# (* ::Text::RGBColor[1, 0, 0]:: *)
# (*Frequenza di osservazione. In Reese et al. 2000, le osservazioni sono a 30 GHZ sia pre BIMA che per OVRO. Per calcolare opprtunamente la correzione relativistica, servirebbe conoscere le frequenze di osservazione nel campione a basso redshift. Nell'articolo di Mason et al. 2001, si utilizzano osservazioni OVRO a 32 GHZ. La correzione relativistica varia per meno dell'1% passando da 30 a 32 GHZ. L'effetto \[EGrave] quindi trascurabile*)
#
#
# \[Nu]OVRO=28.5*10^9;(* frequenza radio di SZ, see Benson et al. 2004, pag. 13*);
# \[Nu]AMIBA=94*10^9;
#
#
# (* ::Subsubsection:: *)
# (*Ellipticity*)
#
#
# f\[Epsilon]Conv[\[Epsilon]_,\[Theta]\[Epsilon]_,\[Theta]sP_]:=
# Which[ 0<\[Epsilon]<1,{\[Epsilon],ArcTan[Tan[\[Theta]\[Epsilon]]],\[Theta]sP}, 1<\[Epsilon]<2,{2-\[Epsilon],ArcTan[Tan[\[Theta]\[Epsilon]]],\[Theta]sP},\[Epsilon]>2,{(\[Epsilon]-2)/(\[Epsilon]-1),ArcTan[Tan[\[Theta]\[Epsilon]+\[Pi]/2]],(\[Epsilon]-1)\[Theta]sP},\[Epsilon]<0,{1-1/(1-\[Epsilon]),ArcTan[Tan[\[Theta]\[Epsilon]-\[Pi]/2]],(1-\[Epsilon])\[Theta]sP},\[Epsilon]==0,{\[Epsilon],\[Theta]\[Epsilon],\[Theta]sP}];
#
# f\[Epsilon]ConvDeg[\[Epsilon]_,\[Theta]\[Epsilon]_,\[Theta]sP_]:=
# Which[ 0<\[Epsilon]<1,{\[Epsilon],RadToDeg*ArcTan[Tan[\[Theta]\[Epsilon]*DegToRad]],\[Theta]sP},
# 1<\[Epsilon]<2,{2-\[Epsilon],RadToDeg*ArcTan[Tan[\[Theta]\[Epsilon]*DegToRad]],\[Theta]sP},
# \[Epsilon]>2,{(\[Epsilon]-2)/(\[Epsilon]-1),RadToDeg*ArcTan[Tan[\[Theta]\[Epsilon]*DegToRad+\[Pi]/2]],(\[Epsilon]-1)\[Theta]sP},
# \[Epsilon]<0,{1-1/(1-\[Epsilon]),RadToDeg*ArcTan[Tan[\[Theta]\[Epsilon]*DegToRad-\[Pi]/2]],(1-\[Epsilon])\[Theta]sP},
# \[Epsilon]==0,{\[Epsilon],\[Theta]\[Epsilon],\[Theta]sP}];
#
#
# (* ::Subsection:: *)
# (*3D Orientation*)
#
#
# (* ::Subsubsection:: *)
# (*Simplifications*)
#
#
# (* ::Text:: *)
# (*Between 0 and 2\[Pi], Sin \[CurlyPhi]>=0*)
#
#
# eSimpl[expr_]:=Simplify[expr,{0<=e<1,0<=\[Epsilon]<1,h>0,eP>=1,0<q1<=1,0<q2<=1,MVir>0,M200>0,cVir>0,c200>0,rs>0,rsP>0}];
# Simpl\[Theta]\[CurlyPhi][expr_]:=Simplify[expr,{0<=Cos\[Theta]<=1,0<=Cos\[CurlyPhi]<=1,\[Psi]\[Element]Reals,0<=Cosi<=1,0<q<=1,0<q1<=1,q1<=q2<=1,0<\[Epsilon]<=1,FacGeo>=0,
# eP>=1,e\[CapitalDelta]>=0,FacGeo83>0,ePSq>1}];
#
# eToqSub={e1->1/q1,e2->1/q2,e3->1};
# qToeSub={q1->e1^-1,q2->e2^-1};
#
# ePTo\[Epsilon]Sub={eP->1/(1-\[Epsilon]),\[CapitalDelta]eP->\[CapitalDelta]\[Epsilon]/(1-\[Epsilon])^2};
# \[Epsilon]ToePSub=Solve[eP==(eP/.ePTo\[Epsilon]Sub),\[Epsilon]]//eSimpl//Flatten;
#
# \[Theta]\[CurlyPhi]ToCosSub={Cos[\[Theta]]->Cos\[Theta],Sin[\[Theta]]->Sqrt[1-Cos\[Theta]^2],Cos[\[CurlyPhi]]->Cos\[CurlyPhi],Sin[\[CurlyPhi]]->Sqrt[1-Cos\[CurlyPhi]^2],Cos[2 \[CurlyPhi]]->2Cos\[CurlyPhi]^2-1,
# Sin[2 \[CurlyPhi]]->2 Cos\[CurlyPhi] Sqrt[1-Cos\[CurlyPhi]^2]
# (*,Cos[\[Psi]]->Cos\[Psi],Sin[\[Psi]]->Sqrt[1-Cos\[Psi]^2],Cos[2 \[Psi]]->2Cos\[Psi]^2-1,Sin[2 \[Psi]]->2 Cos\[Psi] Sqrt[1-Cos\[Psi]^2]*)};
# \[Theta]ToCos\[Theta]Sub={Cos[\[Theta]]->Cos\[Theta],Sin[\[Theta]]->Sqrt[1-Cos\[Theta]^2],Tan[\[Theta]]->Sqrt[1/Cos\[Theta]^2-1],Cot[\[Theta]]->(1/Cos\[Theta]^2-1)^(-1/2),
# Sec[\[Theta]]->1/Cos\[Theta]};
#
#
# (* ::Subsubsection:: *)
# (*Observer's vs intrinsic systems*)
#
#
# (* ::Text:: *)
# (*{\[Theta],\[Phi]} sono gli angoli polari nel sistema intrinseco dell'asse x_{3,obs}. \[Phi]=\[CurlyPhi]-\[Pi]/2*)
#
#
# (* ::Code::GrayLevel[0]:: *)
# (**)
#
#
# IntToObsMatRot=RotationMatrix[Pi-\[Psi],{0,0,1}].RotationMatrix[\[Theta],{1,0,0}].RotationMatrix[Pi-\[CurlyPhi],{0,0,1}]//Simplify;
# ObsToIntMatRot=Inverse[IntToObsMatRot]; (*Inverse[IntToObsMatRot]==Transpose[IntToObsMatRot]=*)
#
# xIntAsObs=ObsToIntMatRot.{x1Obs,x2Obs,x3Obs}//Simplify;
# IntToObsSub={x1Int->xIntAsObs[[1]],x2Int->xIntAsObs[[2]],x3Int->xIntAsObs[[3]]}//Simplify;
#
# x1ObsToInt\[Theta]\[CurlyPhi]=ObsToIntMatRot.{1,0,0}//Simplify;
# x2ObsToInt\[Theta]\[CurlyPhi]=ObsToIntMatRot.{0,1,0}//Simplify;
# x3ObsToInt\[Theta]\[CurlyPhi]=ObsToIntMatRot.{0,0,1}//Simplify;
#
#
# (* ::Subsubsection:: *)
# (*Projected quantities*)
#
#
# (* ::Text:: *)
# (*Between 0 and 2\[Pi], Sin \[CurlyPhi]>=0. \[Psi]_{1,NE} is the position angle of the major axis measured in radians North over East. f_geo and e_P are finally expressed as a function of orientation angles *)
#
#
# (* ::Input:: *)
# (*(**)
# (*TabToPlot=Table[{\[Psi]v,If[(x=\[Psi]v+RadToDeg*(\[Psi]1\[Theta]\[CurlyPhi]/.{\[CurlyPhi]->\[CurlyPhi]v,\[Psi]->DegToRad*\[Psi]v})-DegToRad*\[Psi]v/.eToqSub/.\[Theta]ToCos\[Theta]Sub/.{q1->0.5,q2->0.7,Cos\[Theta]->0.38,\[CurlyPhi]v->-11.4DegToRad})>90,90-x,x]},{\[Psi]v,-90,90,10}]*)
# (*ListPlot[*)
# (*TabToPlot,Joined->True,Frame->True,Axes->False,PlotRange->All*)
# (*]*)
# (**)*)
#
#
# (* ::Input:: *)
# (*(**)
# (*RadToDeg*{\[Psi]1\[Theta]\[CurlyPhi]/.{\[CurlyPhi]->\[CurlyPhi]v,\[Psi]->\[Psi]v},\[Psi]1\[Theta]\[CurlyPhi]/.{\[CurlyPhi]->-\[CurlyPhi]v,\[Psi]->\[Psi]v},(\[Psi]1\[Theta]\[CurlyPhi]/.{\[CurlyPhi]->\[CurlyPhi]v,\[Psi]->\[Psi]v})+(\[Psi]1\[Theta]\[CurlyPhi]/.{\[CurlyPhi]->-\[CurlyPhi]v,\[Psi]->\[Psi]v})+2\[Psi]v}/.eToqSub/.\[Theta]ToCos\[Theta]Sub/.{q1->0.5,q2->0.7,Cos\[Theta]->0.88,\[Psi]v->15.7DegToRad,\[CurlyPhi]v->41.4DegToRad}*)
# (**)*)
#
#
# j\[Zeta]=1/2 ( (e1^2+e2^2) e3^2+(e1^2-e2^2)  e3^2 ( Cos[2 \[CurlyPhi]] (Cos[\[Psi]]^2 Cos[\[Theta]]^2-Sin[\[Psi]]^2) - Cos[\[Theta]] Sin[2 \[CurlyPhi]] Sin[2 \[Psi]] )+ (2 e1^2 e2^2-(e1^2+e2^2) e3^2) Cos[\[Psi]]^2 Sin[\[Theta]]^2    );
# k\[Zeta]=1/4 (
# (-2 e1^2 e2^2+(e1^2+e2^2) e3^2) Sin[\[Theta]]^2 Sin[2 \[Psi]]- (e1^2-e2^2) e3^2 (   2 Cos[\[Theta]] Cos[2 \[Psi]] Sin[2 \[CurlyPhi]] +(1+ Cos[\[Theta]]^2) Cos[2 \[CurlyPhi]]Sin[2 \[Psi]]));
# l\[Zeta]=1/2 ((e1^2+e2^2) e3^2+(2 e1^2 e2^2-(e1^2+e2^2) e3^2) (Sin[\[Psi]]^2 Sin[\[Theta]]^2)+ (e1^2-e2^2) e3^2 (   Cos[\[Theta]] Sin[2 \[CurlyPhi]] Sin[2 \[Psi]]  - Cos[2 \[CurlyPhi]] ( Cos[\[Psi]]^2-Cos[\[Theta]]^2 Sin[\[Psi]]^2)  )  );
# f\[Zeta]=e3^2 Cos[\[Theta]]^2+Sin[\[Theta]]^2 (e2^2 Cos[\[CurlyPhi]]^2+e1^2 Sin[\[CurlyPhi]]^2);
#
# ePjlk=Sqrt[(j+l+Sqrt[(j-l)^2+4k^2])/(j+l-Sqrt[(j-l)^2+4k^2])];
# \[Psi]1jlk=ArcTan[-((-j+Sqrt[4 k^2+(j-l)^2]+l)/(2 k)),1];
# eHB1jlk=(j-Sqrt[4 k^2+(j-l)^2]+l)/(2 f);
# fOnljlkTmp1=(Sin[\[Psi]1jlk]^2+ePjlk^2 Cos[\[Psi]1jlk]^2)^-1;
#
#
# (*ePCos\[Theta]\[CurlyPhi]=ePjlk/.{j->j\[Zeta], k->k\[Zeta],l->l\[Zeta]}/.eToqSub/.\[Theta]ToCos\[Theta]Sub/.\[Psi]->0//Simpl\[Theta]\[CurlyPhi];*)
# ePCos\[Theta]\[CurlyPhi]=\[Sqrt](-((-2+2 Cos\[Theta]^2-q1^2-Cos\[Theta]^2 q1^2-q2^2-Cos\[Theta]^2 q2^2-q1^2 Cos[2 \[CurlyPhi]]+Cos\[Theta]^2 q1^2 Cos[2 \[CurlyPhi]]+q2^2 Cos[2 \[CurlyPhi]]-Cos\[Theta]^2 q2^2 Cos[2 \[CurlyPhi]]-2 q1^2 q2^2 Sqrt[((-1+Cos\[Theta]^2) (-2+q1^2+q2^2)-(1+Cos\[Theta]^2) (q1^2-q2^2) Cos[2 \[CurlyPhi]])^2/(4 q1^4 q2^4)+Cos\[Theta]^2 (1/q1^2-1/q2^2)^2 Sin[2 \[CurlyPhi]]^2])/(2-2 Cos\[Theta]^2+q1^2+Cos\[Theta]^2 q1^2+q2^2+Cos\[Theta]^2 q2^2+q1^2 Cos[2 \[CurlyPhi]]-Cos\[Theta]^2 q1^2 Cos[2 \[CurlyPhi]]-q2^2 Cos[2 \[CurlyPhi]]+Cos\[Theta]^2 q2^2 Cos[2 \[CurlyPhi]]-2 q1^2 q2^2 Sqrt[((-1+Cos\[Theta]^2) (-2+q1^2+q2^2)-(1+Cos\[Theta]^2) (q1^2-q2^2) Cos[2 \[CurlyPhi]])^2/(4 q1^4 q2^4)+Cos\[Theta]^2 (1/q1^2-1/q2^2)^2 Sin[2 \[CurlyPhi]]^2])));
# (*\[Epsilon]Cos\[Theta]\[CurlyPhi]=\[Epsilon]/.\[Epsilon]ToePSub/.eP->ePCos\[Theta]\[CurlyPhi]//Simpl\[Theta]\[CurlyPhi];*)
# \[Epsilon]Cos\[Theta]\[CurlyPhi]=(-1+\[Sqrt](-((-2+2 Cos\[Theta]^2-q1^2-Cos\[Theta]^2 q1^2-q2^2-Cos\[Theta]^2 q2^2-q1^2 Cos[2 \[CurlyPhi]]+Cos\[Theta]^2 q1^2 Cos[2 \[CurlyPhi]]+q2^2 Cos[2 \[CurlyPhi]]-Cos\[Theta]^2 q2^2 Cos[2 \[CurlyPhi]]-2 q1^2 q2^2 Sqrt[((-1+Cos\[Theta]^2) (-2+q1^2+q2^2)-(1+Cos\[Theta]^2) (q1^2-q2^2) Cos[2 \[CurlyPhi]])^2/(4 q1^4 q2^4)+Cos\[Theta]^2 (1/q1^2-1/q2^2)^2 Sin[2 \[CurlyPhi]]^2])/(2-2 Cos\[Theta]^2+q1^2+Cos\[Theta]^2 q1^2+q2^2+Cos\[Theta]^2 q2^2+q1^2 Cos[2 \[CurlyPhi]]-Cos\[Theta]^2 q1^2 Cos[2 \[CurlyPhi]]-q2^2 Cos[2 \[CurlyPhi]]+Cos\[Theta]^2 q2^2 Cos[2 \[CurlyPhi]]-2 q1^2 q2^2 Sqrt[((-1+Cos\[Theta]^2) (-2+q1^2+q2^2)-(1+Cos\[Theta]^2) (q1^2-q2^2) Cos[2 \[CurlyPhi]])^2/(4 q1^4 q2^4)+Cos\[Theta]^2 (1/q1^2-1/q2^2)^2 Sin[2 \[CurlyPhi]]^2]))))/(\[Sqrt](-((-2+2 Cos\[Theta]^2-q1^2-Cos\[Theta]^2 q1^2-q2^2-Cos\[Theta]^2 q2^2-q1^2 Cos[2 \[CurlyPhi]]+Cos\[Theta]^2 q1^2 Cos[2 \[CurlyPhi]]+q2^2 Cos[2 \[CurlyPhi]]-Cos\[Theta]^2 q2^2 Cos[2 \[CurlyPhi]]-2 q1^2 q2^2 Sqrt[((-1+Cos\[Theta]^2) (-2+q1^2+q2^2)-(1+Cos\[Theta]^2) (q1^2-q2^2) Cos[2 \[CurlyPhi]])^2/(4 q1^4 q2^4)+Cos\[Theta]^2 (1/q1^2-1/q2^2)^2 Sin[2 \[CurlyPhi]]^2])/(2-2 Cos\[Theta]^2+q1^2+Cos\[Theta]^2 q1^2+q2^2+Cos\[Theta]^2 q2^2+q1^2 Cos[2 \[CurlyPhi]]-Cos\[Theta]^2 q1^2 Cos[2 \[CurlyPhi]]-q2^2 Cos[2 \[CurlyPhi]]+Cos\[Theta]^2 q2^2 Cos[2 \[CurlyPhi]]-2 q1^2 q2^2 Sqrt[((-1+Cos\[Theta]^2) (-2+q1^2+q2^2)-(1+Cos\[Theta]^2) (q1^2-q2^2) Cos[2 \[CurlyPhi]])^2/(4 q1^4 q2^4)+Cos\[Theta]^2 (1/q1^2-1/q2^2)^2 Sin[2 \[CurlyPhi]]^2]))));
# (*FacGeoCos\[Theta]\[CurlyPhi]=Sqrt[e1 e2 e3]/f\[Zeta]^(3/4)/.eToqSub/.\[Theta]ToCos\[Theta]Sub//Simpl\[Theta]\[CurlyPhi];(* as defined in the AC114 paper*)*)
# FacGeoCos\[Theta]\[CurlyPhi]=1/(Sqrt[q1 q2] (Cos\[Theta]^2+(1-Cos\[Theta]^2) (Cos[\[CurlyPhi]]^2/q2^2+Sin[\[CurlyPhi]]^2/q1^2))^(3/4));
# e\[CapitalDelta]Cos\[Theta]\[CurlyPhi]=ePCos\[Theta]\[CurlyPhi]^(1/2)/FacGeoCos\[Theta]\[CurlyPhi];
#
# \[Psi]1\[Theta]\[CurlyPhi]=\[Psi]1jlk/.{j->j\[Zeta], k->k\[Zeta],l->l\[Zeta]};
# Inv\[Psi]Binney\[Theta]\[CurlyPhi]=(j-l)/(2 k)/.{k->k\[Zeta],l->l\[Zeta],j->j\[Zeta]}/.eToqSub/.\[Theta]ToCos\[Theta]Sub//Simpl\[Theta]\[CurlyPhi];
# \[Psi]1NECos\[Theta]\[CurlyPhi](* to measure from NE \[Pi]/2 must be subtracted*)=-(\[Pi]/2)+ArcTan[(-(-1+Cos\[Theta])^2 (q1^2-q2^2) Cos[2 \[CurlyPhi]-2 \[Psi]]+(1+Cos\[Theta]) (2 (-1+Cos\[Theta]) (-2+q1^2+q2^2) Cos[2 \[Psi]]-(1+Cos\[Theta]) (q1^2-q2^2) Cos[2 (\[CurlyPhi]+\[Psi])]))/(2 (2 Cos\[Theta] (q1^2-q2^2) Cos[2 \[Psi]] Sin[2 \[CurlyPhi]]+(-(-1+Cos\[Theta]^2) (-2+q1^2+q2^2)+(1+Cos\[Theta]^2) (q1^2-q2^2) Cos[2 \[CurlyPhi]]) Sin[2 \[Psi]]))-Sign[(1-Cos\[Theta]^2) (1/q1^2+1/q2^2-2/(q1^2 q2^2)) Sin[2 \[Psi]]-(1/q1^2-1/q2^2) (2 Cos\[Theta] Cos[2 \[Psi]] Sin[2 \[CurlyPhi]]+(1+Cos\[Theta]^2) Cos[2 \[CurlyPhi]] Sin[2 \[Psi]])] Sqrt[1+(-(-1+Cos\[Theta])^2 (q1^2-q2^2) Cos[2 \[CurlyPhi]-2 \[Psi]]+(1+Cos\[Theta]) (2 (-1+Cos\[Theta]) (-2+q1^2+q2^2) Cos[2 \[Psi]]-(1+Cos\[Theta]) (q1^2-q2^2) Cos[2 (\[CurlyPhi]+\[Psi])]))^2/(4 (2 Cos\[Theta] (q1^2-q2^2) Cos[2 \[Psi]] Sin[2 \[CurlyPhi]]+(-(-1+Cos\[Theta]^2) (-2+q1^2+q2^2)+(1+Cos\[Theta]^2) (q1^2-q2^2) Cos[2 \[CurlyPhi]]) Sin[2 \[Psi]])^2)],1];
# (* ArcTan[(Inv\[Psi]Binney\[Theta]\[CurlyPhi]-Sign[k]Sqrt[1+Inv\[Psi]Binney\[Theta]\[CurlyPhi]^2]),1]-\[Pi]/2/.{k->k\[Zeta],l->l\[Zeta],j->j\[Zeta]}/.eToqSub/.\[Theta]ToCos\[Theta]Sub;*)
# (*(\[Psi]1jlk-\[Pi]/2)/.{k->k\[Zeta],l->l\[Zeta],j->j\[Zeta]}/.{e1->1/q1,e2->1/q2,e3->1}/.\[Theta]ToCos\[Theta]Sub//Simpl\[Theta]\[CurlyPhi]; *)
#
#
# FacGeo43Cos\[Theta]Cos\[CurlyPhi]=FacGeoCos\[Theta]\[CurlyPhi]^(4/3)/.\[Theta]\[CurlyPhi]ToCosSub//Simpl\[Theta]\[CurlyPhi];
# ePSqCos\[Theta]Cos\[CurlyPhi]=((1-Cos\[Theta]^2)/(q1^2 q2^2)+Cos\[Theta]^2 (1/q1^2+1/q2^2)+(1-Cos\[Theta]^2)((1-Cos\[CurlyPhi]^2)/q1^2+Cos\[CurlyPhi]^2/q2^2)+ Sqrt[4 Cos\[Theta]^2 Cos\[CurlyPhi]^2 (1-Cos\[CurlyPhi]^2) (1/q1^2-1/q2^2)^2+((1-Cos\[Theta]^2)/(q1^2 q2^2)+Cos\[Theta]^2 (1/q1^2+1/q2^2)-(1+Cos\[Theta]^2)((1-Cos\[CurlyPhi]^2)/q1^2+Cos\[CurlyPhi]^2/q2^2))^2])/((1-Cos\[Theta]^2)/(q1^2 q2^2)+Cos\[Theta]^2 (1/q1^2+1/q2^2)+(1-Cos\[Theta]^2)((1-Cos\[CurlyPhi]^2)/q1^2+Cos\[CurlyPhi]^2/q2^2)-Sqrt[4 Cos\[Theta]^2 Cos\[CurlyPhi]^2 (1-Cos\[CurlyPhi]^2) (1/q1^2-1/q2^2)^2+((1-Cos\[Theta]^2)/(q1^2 q2^2)+Cos\[Theta]^2 (1/q1^2+1/q2^2)-(1+Cos\[Theta]^2)((1-Cos\[CurlyPhi]^2)/q1^2+Cos\[CurlyPhi]^2/q2^2))^2]);
# e\[CapitalDelta]Cos\[Theta]Cos\[CurlyPhi]=ePSqCos\[Theta]Cos\[CurlyPhi]^(1/4)/FacGeo43Cos\[Theta]Cos\[CurlyPhi]^(3/4);
#
#
# (* ::Input:: *)
# (*(*eP\[Theta]\[CurlyPhi]/ePSqCos\[Theta]Cos\[CurlyPhi]^(1/2)/.{q1->0.6,q2->0.9,Cos\[Theta]->0.63,\[CurlyPhi]->\[CurlyPhi]v,Cos\[CurlyPhi]->Cos[\[CurlyPhi]v]}/.\[CurlyPhi]v->10*DegToRad*)*)
#
#
# (* ::Text:: *)
# (*Ellipticity, elongation and orientation in the sky are invariant for \[Theta]->-\[Theta]. We can then express \[Theta] in terms of Cos \[Theta]*)
#
#
# (* ::Input:: *)
# (*(*{ePjlk,Sqrt[e1 e2 e3]/f\[Zeta]^(3/4),(ArcTan[((j-l)/(2 k)-k/Abs[k] Sqrt[1+((j-l)/(2 k))^2]),1]-\[Pi]/2)}/.{j->j\[Zeta], k->k\[Zeta],l->l\[Zeta]}/.eToqSub/.{q1->0.7,q2->0.78,\[Theta]->{1,-1}32*DegToRad,\[CurlyPhi]->-\[Pi]/3,\[Psi]->11DegToRad}*)*)
#
#
# (* ::Text:: *)
# (*Ellipticity and elongation are invariant for \[CurlyPhi]->-\[CurlyPhi]. On the other hand, the orientation in the plane of the sky depends on the sign on \[CurlyPhi].*)
#
#
# (* ::Input:: *)
# (*(*{ePjlk,Sqrt[e1 e2 e3]/f\[Zeta]^(3/4),(j-l)/(2 k),(ArcTan[((j-l)/(2 k)-k/Abs[k] Sqrt[1+((j-l)/(2 k))^2]),1](*-\[Pi]/2*))*DegToRad}/.{j->j\[Zeta], k->k\[Zeta],l->l\[Zeta]}/.eToqSub/.{q1->0.7,q2->0.78,\[Theta]->32*DegToRad,\[CurlyPhi]->{1,-1}\[Pi]/2.5,\[Psi]->11DegToRad}*)*)
#
#
# (* ::Subsubsection:: *)
# (*Scale length*)
#
#
# FacGeoTofSub={FacGeo->1/(f^(3/4) Sqrt[q1 q2]) };
# FacGeoToe\[CapitalDelta]Sub={FacGeo->eP^(1/2)/e\[CapitalDelta]};
#
# e\[CapitalDelta]TofSub=e\[CapitalDelta]->(eP/(e1 e2))^(1/2) f^(3/4);
# fToe\[CapitalDelta]Sub=Solve[(e\[CapitalDelta]/.e\[CapitalDelta]TofSub)^4==e\[CapitalDelta]^4,f][[1]]//Simplify;
# e\[CapitalDelta]ToFacGeoSub={e\[CapitalDelta]->eP^(1/2)/FacGeo,\[CapitalDelta]e\[CapitalDelta]->Sqrt[(D[eP^(1/2)/FacGeo,eP]\[CapitalDelta]eP)^2+(D[eP^(1/2)/FacGeo,FacGeo]\[CapitalDelta]FacGeo)^2]};
# rcTo\[Theta]PSub=rc->Dd Sqrt[f]/e\[CapitalDelta] \[Theta]P;
# rcTorPSub=rc->(Sqrt[f] rP)/e\[CapitalDelta];
# rsTorsPSub=rs->rsP*Sqrt[f]/e\[CapitalDelta];
# rsPTorcSub=rsP->rc*(Sqrt[f]/e\[CapitalDelta])^-1;
# rsPTorsSub=rsP->rs*(Sqrt[f]/e\[CapitalDelta])^-1;
# rsFacGeoTofSub={rsFacGeo->1/(f^(1/4) Sqrt[q1 q2]) };
#
# \[Zeta]sTo\[Theta]Sub=\[Zeta]s->Dd Sqrt[\[Theta]1^2+eP^2 \[Theta]2^2](rc/rP)/.rcTo\[Theta]PSub/.rP->Dd \[Theta]P;
#
# \[Zeta]vToIntSub=\[Zeta]v->Sqrt[e1^2 x1Int^2+e2^2 x2Int^2+e3^2 x3Int^2];
# \[Xi]vOnrPvTmp1=Sqrt[(x1\[Psi]^2+eP^2 x2\[Psi]^2)/rP^2]/.{x1\[Psi]->x1Obs Cos[\[Psi]P]+x2Obs Sin[\[Psi]P],x2\[Psi]->-x1Obs Sin[\[Psi]P]+x2Obs Cos[\[Psi]P]};
#
#
# (* ::Input:: *)
# (*(**)
# (*EqTmp1={CosSq+ePSq SinSq==jOnf,SinSq+ePSq CosSq==lOnf};*)
# (*SolTmp1=Solve[EqTmp1,{CosSq,SinSq}][[1]]//Simplify*)
# (*EqTmp1/.SolTmp1//Simplify*)
# (**)*)
#
#
# (* ::Input:: *)
# (*(*Collect[D[\[Zeta]v^2/.\[Zeta]vToIntSub/.IntToOssSub,x1Obs]^2+D[\[Zeta]v^2/.\[Zeta]vToIntSub/.IntToOssSub,x2Obs]^2/.\[Psi]->0,x3Obs,Simplify];*)*)
#
#
# (* ::Input:: *)
# (*(*D[\[Zeta]v^2/.\[Zeta]vToIntSub/.IntToOssSub,x1Obs]//Simplify*)*)
#
#
# (* ::Subsubsection:: *)
# (*Ellipticity, axial ratios,...*)
#
#
# (* ::Text:: *)
# (*T^Mat = Sqrt[1 - q_ 2^2]/Sqrt[1 - q_ 1^2]; T^ICM = Sqrt[1 - q_ 2^(ICM 2)]/Sqrt[1 - q_ 1^(ICM 2)]; T^Mat = T^ICM -> Sqrt[1 - q_ 2^2]/Sqrt[1 - q_ 2^(ICM 2)] = Sqrt[1 - q_ 1^2]/Sqrt[1 - q_ 1^(ICM 2)] -> Sqrt[1 - q_ 1^(ICM 2)] e_ 2/e_ 2^ICM = Sqrt[1 - q_ 1^2] (Sqrt[1 - q_ 2^2] Subscript[(e^ICM), 1]/e_ 1 = Sqrt[1 - q_ 2^(ICM 2)])*)
# (*Subscript[(e^ICM), 2]/Subscript[(e^ICM), 1] = e_ 2/e_ 1 -> Subscript[(e^ICM), 2]/e_ 2 = (Subscript[(e^ICM), 1]/e_ 1) *)
#
#
# qICMToqDMSubTmp1=Simplify[Solve[eICMOneMat Sqrt[1-qDM^2]==Sqrt[1-qICM^2],qICM],eICMOneMat>0][[2]]
# qICMToqSub={qICM->Sqrt[1-eICMOneMat^2 (1-q^2)],qICM1->Sqrt[1-eICMOneMat^2 (1-q1^2)],qICM2->Sqrt[1-eICMOneMat^2 (1-q2^2)]};
#
# qToqICMSubTmp1=Simplify[Solve[eICMOneMat Sqrt[1-q^2]==Sqrt[1-qICM^2],q],eICMOneMat>0][[2]];
# qToqICMSub={q->Sqrt[1-(1-qICM^2)/eICMOneMat^2],q1->Sqrt[1-(1-qICM1^2)/eICMOneMat^2],q2->Sqrt[1-(1-qICM2^2)/eICMOneMat^2]};
#
#
# (* ::Input:: *)
# (*(*Solve[(1-q^2)/(1-qICM^2)==eICMOneMat^2,qICM]//Simplify*)*)
#
#
# (* ::Input:: *)
# (*(*Solve[(1-qICM2^2)/(1-qICM1^2)==(1-q2^2)/(1-q1^2),qICM2][[2]]//Simplify*)*)
# (*(*Sqrt[1-(1-q2^2)/(1-q1^2) (1-qICM1^2)]/(Sqrt[q2^2-q1^2+qICM1^2 (1-q2^2)]/Sqrt[1-q1^2])/.{q1->0.5,q2->0.8,qICM1->0.6}*)*)
#
#
# qICM2TFixedSub=qICM2->Sqrt[1-(1-q2^2)/(1-q1^2) (1-qICM1^2)];
#
# eICMONeMatToq1Sub=eICMONeMat->Sqrt[(1-qICM1^2)/(1-q1^2)];
#
#
# (* ::Text:: *)
# (*Checks*)
#
#
# (* ::Input:: *)
# (*(**)
# (*D[qICM/.qICMToqSub,q]*D[qICM/.qICMToqSub,eICMOneMat]//Simplify*)
# (**)*)
#
#
# (* ::Input:: *)
# (*(**)
# (*Timing[qICMRandomTab=Table[qICM/.qICMToqSub/.{q->RandomReal[],eICMOneMat->RandomReal[]},{i,10^5}];]*)
# (*Histogram[qICMRandomTab]*)
# (**)*)
#
#
# (* ::Input:: *)
# (*(**)
# (*EulSubv={\[Psi]->0DegToRad,\[CurlyPhi]->-60*DegToRad,Cos\[Theta]->0.9};*)
# (*qSubv={q1->0.5,q2->0.8};*)
# (**)
# (*DegToRad^-1*\[Psi]1NECos\[Theta]\[CurlyPhi]/.EulSubv/.qSubv*)
# (*DegToRad^-1*\[Psi]1NECos\[Theta]\[CurlyPhi]/.EulSubv/.{q1->qICM1,q2->qICM2}/.qICMToqSub/.qSubv//Simplify*)
# (**)*)
#
#
# (* ::Section:: *)
# (*ICM models*)
#
#
# (* ::Subsubsection:: *)
# (*Electron density*)
#
#
# ne\[Beta]=n0 (1+(\[Zeta]v/rc)^2)^(-((3\[Beta])/2));
# ne\[Beta]Proj=n0 (1+(\[Xi]v/rP)^2)^(1/2-(3\[Beta])/2);
# ne\[Beta]SqProj=n0Sq (1+(\[Xi]v/rP)^2)^(1/2-3\[Beta]);
# SB\[Beta]Proj=SB0 (1+(\[Xi]v/rP)^2)^(1/2-3\[Beta]);
# neET09=n0 (\[Zeta]v/rc)^-\[Eta] (1+(\[Zeta]v/rc)^2)^(-((3\[Beta])/2)+\[Eta]/2) (1+(\[Zeta]v/rt)^a5)^(-\[Gamma]ICM/3)/.{a5->3(*,\[Gamma]ICM->1.2*)};
#
#
# (* ::Subsubsection:: *)
# (*Temperature profiles*)
#
#
# (* ::Text:: *)
# (*Temperatures: Subscript[T, 0]and Subscript[T, Min]; scale lengths: Subscript[r, cT]and Subscript[r, cool]; stepness parameters: Subscript[\[Alpha], Cool], Subscript[a, T], Subscript[b, T], Subscript[c, T]*)
#
#
# TVK=T0 (r/rcT)^-aT/(1+(r/rcT)^bT)^(cT/bT) (x+TMin/T0)/(x+1)/.x->(r/rCool)^\[Alpha]Cool/.r->\[Zeta]v;
# TVKSimp=TVK/.{aT->0.,bT->2.};
# TVKaTSimp=TVK/.{bT->2.};
# TVKNoCoolCoreSimp=TVK/.{aT->0.,bT->2.,TMin->T0}//Simplify;
# TVKUniv=TVK/.{\[Alpha]Cool->1.9,rCool->0.045/0.6 rcT ,TMin->0.45T0,aT->0,bT->2,cT->0.45*2};
# (*TVKUnivf[\[Zeta]v_]=T0 (r/rcT)^-aT/(1+(r/rcT)^bT)^(cT/bT) (x+(TMin/T0))/(x+1)/.x->(r/rCool)^\[Alpha]Cool/.r->\[Zeta]v/.{\[Alpha]Cool->1.9,rCool->0.045/0.6 rcT ,TMin->0.45T0,aT->0,bT->2,cT->0.45*2};*)
#
#
# (* ::Subsubsection:: *)
# (*Profiles*)
#
#
# (* ::Text:: *)
# (*In terms of the dimensionless unit x=r/Subscript[r, c]. Subscript[r, c]is expressed in arcminuts*)
#
#
# neET09xDimless[x_]:=n0*x^-\[Eta]*(1+x^2)^(-((3 \[Beta])/2)+\[Eta]/2) (1+x^3/rtOnrc^3)^(-\[Gamma]ICM/3)(*neET09/.{rt->rc*rtOnrc}/.{\[Zeta]v->x*rc}*);
# nePA02xDimless[x_]:=n0*x^-\[Eta]*(1+x^2)^(-((3 \[Beta])/2)+\[Eta]/2)(*neET09/.{rt->rc*rtOnrc,\[Gamma]ICM\[Rule]0}/.{\[Zeta]v->x*rc}*);
# ne\[Beta]xDimless[x_]:=ne\[Beta]/.{\[Zeta]v->x*rc}
# TVKUnivDimless[x_]:=TVKUniv/.{rcT->rc*rcTOnrc}/.{\[Zeta]v->x*rc};
# TVKSimpDimless[x_]:=TVKSimp/.{rcT->rc*rcTOnrc,rCool->rc*rCoolOnrc}/.{\[Zeta]v->x*rc};
# TVKaTSimpDimless[x_]:=TVKaTSimp/.{rcT->rc*rcTOnrc,rCool->rc*rCoolOnrc}/.{\[Zeta]v->x*rc};
#
# n\[Beta]ZeDimless[x_]:=ne\[Beta]/.{rc->rc*rcZOnrc,n0->n0Z,\[Beta]->\[Beta]Z}/.{\[Zeta]v->x*rc};
# (*n\[Beta]ZeDimlessLM08Sub={\[Beta]Z->\[Beta],rcZOnrc->rcZ/rc}/.rcZ->Abs[rP]*R180/.MetProfFit["BestFitParameters"]/.R180->RadToArcMin/DACluster 1780(T/(5 keV))^(1/2) H0/Hubble\[CapitalLambda][zdCluster,0.3,1-0.3] kpc/.T->10keV/.SubMKS/.SubMKS;*)
#
#
# (* ::Section:: *)
# (*Elliptical profiles*)
#
#
# (* ::Text:: *)
# (*Subscript[\[Theta], \[Epsilon]]is the orientation angle North over East. \[Epsilon]=1-q*)
#
#
# \[Theta]RotSub={\[Theta]1->(RotationMatrix[-\[Theta]\[Epsilon]+\[Pi]/2].{\[Theta]1,\[Theta]2})[[1]],\[Theta]2->(RotationMatrix[-\[Theta]\[Epsilon]+\[Pi]/2].{\[Theta]1,\[Theta]2})[[2]]};
# \[Theta]TraSub={\[Theta]1->\[Theta]1-\[Theta]10,\[Theta]2->\[Theta]2-\[Theta]20};
# \[Theta]EllSub={\[Theta]Ell->Sqrt[\[Theta]1^2+\[Theta]2^2/(1-\[Epsilon])^2]};
# \[Theta]EllRoTra=(\[Theta]Ell/.\[Theta]EllSub/.\[Theta]RotSub/.\[Theta]TraSub);
#
#
# (* ::Section:: *)
# (*IS*)
#
#
# (* ::Subsection:: *)
# (*Isothermal ellipsoid*)
#
#
# (* ::Text:: *)
# (*Subscript[\[Theta], \[Epsilon]]is the oritntation anle measured North over East. \[Epsilon]=1-b/a*)
#
#
# kISRotTra=b/(2Sqrt[\[Theta]c^2+\[Theta]Ell^2])/.\[Theta]EllSub/.\[Theta]RotSub/.\[Theta]TraSub;
#
#
# (* ::Input:: *)
# (*(**)
# (*{xv,\[Gamma]v}={10^-5,2.5};*)
# (*xv^(1.-\[Gamma]v) ((1.+xv)^(\[Gamma]v-3.)+(3.-\[Gamma]v)NIntegrate[(y+xv)^(\[Gamma]v-4.) (1-Sqrt[1-y^2]),{y,0.,1.}])*)
# (**)*)
#
#
# (* ::Section:: *)
# (*NFW profile*)
#
#
# \[Rho]NFW=\[Rho]s/((\[Zeta]/rs)(1+\[Zeta]/rs)^2);
#
#
# (* ::Text:: *)
# (*Projected density inside the scale radius (x<1)*)
#
#
# f\[Theta]EllRotTraDeg=Compile[{{\[Theta]1,_Real},{\[Theta]2,_Real},{\[Theta]10,_Real},{\[Theta]20,_Real},{\[Epsilon],_Real},{\[Theta]\[Epsilon],_Real}},\[Sqrt]((-(\[Theta]2-\[Theta]20) Cos[0.017453292519943295` \[Theta]\[Epsilon]]+(\[Theta]1-\[Theta]10) Sin[0.017453292519943295` \[Theta]\[Epsilon]])^2+((\[Theta]1-\[Theta]10) Cos[0.017453292519943295` \[Theta]\[Epsilon]]+(\[Theta]2-\[Theta]20) Sin[0.017453292519943295` \[Theta]\[Epsilon]])^2/(1. -\[Epsilon])^2),
# RuntimeAttributes->Listable,Parallelization->True,CompilationTarget->"C"];
#
# f\[Theta]EllRotTraDegTmp1=Compile[{{\[Theta],_Real,1,2},{\[Theta]10,_Real},{\[Theta]20,_Real},{\[Epsilon],_Real},{\[Theta]\[Epsilon],_Real}},\[Sqrt]((-(\[Theta][[2]]-\[Theta]20) Cos[0.017453292519943295` \[Theta]\[Epsilon]]+(\[Theta][[1]]-\[Theta]10) Sin[0.017453292519943295` \[Theta]\[Epsilon]])^2+((\[Theta][[1]]-\[Theta]10) Cos[0.017453292519943295` \[Theta]\[Epsilon]]+(\[Theta][[2]]-\[Theta]20) Sin[0.017453292519943295` \[Theta]\[Epsilon]])^2/(1. -\[Epsilon])^2),RuntimeAttributes->Listable,Parallelization->True,CompilationTarget->"C"];
#
#
# fkNFWx2=Compile[{{x1,_Real},{x2,_Real},{x01,_Real},{x02,_Real},{ks,_Real},{rsP,_Real},{\[Epsilon],_Real},{\[Theta]\[Epsilon],_Real}},Block[{xEll,Cos\[Theta]\[Epsilon],Sin\[Theta]\[Epsilon]},
# Cos\[Theta]\[Epsilon]=Cos[0.017453292519943295`*\[Theta]\[Epsilon]];
# Sin\[Theta]\[Epsilon]=Sin[0.017453292519943295`*\[Theta]\[Epsilon]];
# xEll=Sqrt[((x1-x01) Sin\[Theta]\[Epsilon]-(x2-x02) Cos\[Theta]\[Epsilon])^2+((x1-x01) Cos\[Theta]\[Epsilon]+(x2-x02) Sin\[Theta]\[Epsilon])^2/(1. -\[Epsilon])^2]/rsP;
# Return[(2. ks)/(xEll^2-1.) If[xEll<1.,1.-(2. ArcTanh[Sqrt[(1.-xEll)/(1.+xEll)]])/Sqrt[1.-xEll^2],1.-(2. ArcTan[Sqrt[(xEll-1.)/(1.+xEll)]])/Sqrt[xEll^2-1.]]]
# ],
# RuntimeAttributes->Listable,CompilationTarget->"C",RuntimeOptions->"Speed",Parallelization->True];
#
#
# fkNFWx2Uncompiled[x1_,x2_,x01_,x02_,ks_,rsP_,\[Epsilon]_,\[Theta]\[Epsilon]_]:=Block[{xEll,Cos\[Theta]\[Epsilon],Sin\[Theta]\[Epsilon]},
# Cos\[Theta]\[Epsilon]=Cos[0.017453292519943295`*\[Theta]\[Epsilon]];
# Sin\[Theta]\[Epsilon]=Sin[0.017453292519943295`*\[Theta]\[Epsilon]];
# xEll=Sqrt[((x1-x01) Sin\[Theta]\[Epsilon]-(x2-x02) Cos\[Theta]\[Epsilon])^2+((x1-x01) Cos\[Theta]\[Epsilon]+(x2-x02) Sin\[Theta]\[Epsilon])^2/(1. -\[Epsilon])^2]/rsP;
# Return[(2. ks)/(xEll^2-1.) If[xEll<1.,1.-(2. ArcTanh[Sqrt[(1.-xEll)/(1.+xEll)]])/Sqrt[1.-xEll^2],1.-(2. ArcTan[Sqrt[(xEll-1.)/(1.+xEll)]])/Sqrt[xEll^2-1.]]]
# ];
# SetAttributes[fkNFWx2Uncompiled,Listable];
#
#
# (*convergence*)
# fkNFWxle1=Compile[{{x,_Real}}, 2./(x^2-1) (1.-1./Sqrt[1.-x^2] ArcCosh[1./x]),RuntimeAttributes->Listable,CompilationTarget->"C"];
# fkNFWxeq1=2./3.;
# fkNFWxge1=Compile[{{x,_Real}},2./(x^2-1) (1-1/Sqrt[x^2-1] ArcCos[1/x]),RuntimeAttributes->Listable,CompilationTarget->"C"];
# fkNFWOnks=Compile[{{x,_Complex}},Re[ 2./(x^2-1) (1.-1./Sqrt[1.-x^2] ArcCosh[1./x])],RuntimeAttributes->Listable,CompilationTarget->"C"];
#
# fkNFWOnksTmp1=Compile[{{x,_Real}},
# If[
# x<1,2./(x^2-1) (1.-1./Sqrt[1.-x^2] ArcCosh[1./x]),2./(x^2-1) (1-1/Sqrt[x^2-1] ArcCos[1/x])]
# (*Which[
# x<1,2./(x^2-1) (1.-1./Sqrt[1.-x^2] ArcCosh[1./x]),
# x>1,2./(x^2-1) (1-1/Sqrt[x^2-1] ArcCos[1/x]),
# x==1,2./3]*),RuntimeAttributes->Listable,CompilationTarget->"C"];
#
#
#
# (*fkNFWOnks=Function[x,Which[x<1,fkNFWxle1[x],x>1,fkNFWxge1[x],x==1,fkNFWxeq1],{Listable}];*)
#
# fkNFWx[x_,ks_]:=ks*Which[x<1,fkNFWxle1[x],x>1,fkNFWxge1[x],x==1,fkNFWxeq1];
# fkNFWSph[r_,ks_,rs_]:=ks*Which[r<rs,fkNFWxle1[r/rs],r>rs,fkNFWxge1[r/rs],r==rs,fkNFWxeq1];
#
# (*shear*)
# f\[Gamma]NFWxle1=Compile[{{x,_Real}},8./(x^2 Sqrt[1.-x^2]) ArcTanh[Sqrt[(1.-x)/(1.+x)]]+4./x^2 Log[x/2.]- 2./(x^2-1.)+4./((x^2-1.)(1.-x^2)^(1/2)) ArcTanh[Sqrt[(1.-x)/(1.+x)]],RuntimeAttributes->Listable,CompilationTarget->"C"];
# f\[Gamma]NFWxeq1=10./3.+4.Log[0.5];
# f\[Gamma]NFWxge1=Compile[{{x,_Real}},8./(x^2 Sqrt[x^2-1.]) ArcTan[Sqrt[(x-1.)/(x+1.)]]+4./x^2 Log[x/2.]- 2./(x^2-1.)+4./(x^2-1)^(3/2) ArcTan[Sqrt[(x-1.)/(x+1.)]],RuntimeAttributes->Listable,CompilationTarget->"C"];
# f\[Gamma]tNFWSph=Function[{r,ks,rs},ks*Which[r<rs,f\[Gamma]NFWxle1[r/rs],r>rs,f\[Gamma]NFWxge1[r/rs],r==rs,f\[Gamma]NFWxeq1],Listable];
#
# fgtNFWSph=Compile[
# {{r,_Real},{ks,_Real},{rs,_Real}},f\[Gamma]tNFWSph[r,ks,rs]/(1-fkNFWSph[r,ks,rs]),RuntimeAttributes->Listable
# ];
#
#
# kNFWRotTra=Re[(2ks 1/(x^2-1) (1-1/Sqrt[1-x^2] ArcCosh[1/x])/.x->\[Theta]Ell/\[Theta]sP/.\[Theta]EllSub/.\[Theta]RotSub)/.\[Theta]TraSub];
# MassNFWRotTra=Re[(4ks(Log[x/2]+2/Sqrt[1-x^2] ArcTanh[Sqrt[(1-x)/(1+x)]])/.x->\[Theta]Ell/\[Theta]sP/.\[Theta]EllSub/.\[Theta]RotSub)/.\[Theta]TraSub];
# FCorKee[x_]:=1/Sqrt[1-x^2] ArcTanh[Sqrt[1-x^2]];
# kNFWRotTraKee=2ks (1-FCorKee[x])/(x^2-1)/.x->\[Theta]Ell/\[Theta]sP/.\[Theta]EllSub/.\[Theta]RotSub/.{\[Theta]1->\[Theta]1-\[Theta]10,\[Theta]2->\[Theta]2-\[Theta]20};
#
#
# (* ::Input:: *)
# (*(**)
# (*fkNFWrTmp1=Compile[{{r,_Real},{ks,_Real},{rs,_Real}},*)
# (*ks*Which[r<rs,fkNFWxle1[r/rs],r>rs,fkNFWxge1[r/rs],r==rs,fkNFWxeq1]*)
# (*];*)
# (**)*)
#
#
# (* ::Subsubsection:: *)
# (*Shear*)
#
#
# (* ::Text:: *)
# (*WRIGHT & BRAINERD (2000). Subscript[k, s]=Subscript[r, s] Subscript[\[Delta], c]Subscript[\[Rho], Cr]/Subscript[\[CapitalSigma], Cr]*)
#
#
# (* ::Input:: *)
# (*(**)
# (*f\[Gamma]tNFWSphTmp1=Compile[{{r,_Real},{ks,_Real},{rs,_Real}},*)
# (*Block[{x=r/rs},ks*(8./(x^2 Sqrt[1.-x^2]) ArcTanh[Sqrt[(1.-x)/(1.+x)]]+4./x^2 Log[x/2.]- 2./(x^2-1.)+4./((x^2-1.)(1.-x^2)^(1/2)) ArcTanh[Sqrt[(1.-x)/(1.+x)]])],RuntimeAttributes->Listable,CompilationTarget->"C"];*)
# (**)*)
#
#
# (* ::Input:: *)
# (*(**)
# (*{\[Theta]Tmp1,ksTmp1,\[Theta]sTmp1}={40,1.,1.};*)
# (*\[Gamma]NFWTangSph/.{\[Theta]->\[Theta]Tmp1,ks->ksTmp1,\[Theta]s->\[Theta]sTmp1}*)
# (*f\[Gamma]tNFWSph[\[Theta]Tmp1,ksTmp1,\[Theta]sTmp1]*)
# (**)*)
#
#
# (* ::Input:: *)
# (*(**)
# (*LogLogPlot[f\[Gamma]tNFWSph[\[Theta],ksTmp1,\[Theta]sTmp1],{\[Theta],10^-2,10^2},PlotRange->All]*)
# (**)*)
#
#
# (* ::Input:: *)
# (*(**)
# (*{\[Theta]Tmp1,ksTmp1,\[Theta]sTmp1}={1.1,1.,1.};*)
# (*AbsoluteTiming[Table[f\[Gamma]tNFWSph[\[Theta]Tmp1,ksTmp1,\[Theta]sTmp1],{i,10^4}];]*)
# (*AbsoluteTiming[Table[\[Gamma]NFWTangSph/.{\[Theta]->\[Theta]Tmp1,ks->ksTmp1,\[Theta]s->\[Theta]sTmp1},{i,10^4}];]*)
# (**)*)
#
#
# kNFWSphToPlot=2ks*(1/(x^2-1) (1-2/Sqrt[1-x^2] ArcTanh[Sqrt[(1-x)/(1+x)]])/.x->\[Theta]/\[Theta]s);
# kNFWSph=Re[kNFWSphToPlot];
# (*kNFWSph=2ks*Re[1/(x^2-1) (1-2/Sqrt[1-x^2] ArcTanh[Sqrt[(1-x)/(1+x)]])/.x->\[Theta]/\[Theta]s];*)
# kNFWWhitinToPlot=4ks*(1/x^2 (2/Sqrt[1-x^2] ArcTanh[Sqrt[(1-x)/(1+x)]]+Log[x/2])/.x->\[Theta]/\[Theta]s);
# kNFWWhitin=Re[kNFWWhitinToPlot];
# kNFWWithinToPlot=kNFWWhitinToPlot;
#
#
# \[Gamma]NFWTangSphToPlot=ks*((8/(x^2 Sqrt[1-x^2]) ArcTanh[Sqrt[(1-x)/(1+x)]]+4/x^2 Log[x/2]- 2/(x^2-1)+4/((x^2-1)(1-x^2)^(1/2)) ArcTanh[Sqrt[(1-x)/(1+x)]])/.x->\[Theta]/\[Theta]s);
# \[Gamma]NFWTangSph=Re[\[Gamma]NFWTangSphToPlot];
#
#
# gNFWTangSphToPlot=\[Gamma]NFWTangSphToPlot/(1-kNFWSphToPlot)//Simplify(*/.{\[Theta]10->0,\[Theta]20->0}/.{\[Theta]sP->\[Theta]s,\[Theta]1->\[Theta],\[Theta]2->0,\[Epsilon]->0,\[Theta]\[Epsilon]->0}*);
# gNFWTangSph=\[Gamma]NFWTangSph/(1-kNFWSph)//Simplify(*/.{\[Theta]10->0,\[Theta]20->0}/.{\[Theta]sP->\[Theta]s,\[Theta]1->\[Theta],\[Theta]2->0,\[Epsilon]->0,\[Theta]\[Epsilon]->0}*);
#
#
# (*dgNFWTangSphOnd\[Theta]=Re[D[\[Gamma]NFWTangSphToPlot/(1-kNFWSphToPlot),\[Theta]]//Simplify]*)
# dgNFWTangSphOnd\[Theta]=Re[(2 ks (32 ks Sqrt[1-\[Theta]^2/\[Theta]s^2] \[Theta]s^6 Sqrt[1-(2 \[Theta])/(\[Theta]+\[Theta]s)] ArcTanh[Sqrt[1-(2 \[Theta])/(\[Theta]+\[Theta]s)]]^2+2 (\[Theta]-\[Theta]s) (\[Theta]+\[Theta]s) Sqrt[1-(2 \[Theta])/(\[Theta]+\[Theta]s)] ArcTanh[Sqrt[1-(2 \[Theta])/(\[Theta]+\[Theta]s)]] (9 \[Theta]^4-2 (5+4 ks) \[Theta]^2 \[Theta]s^2+4 (1+3 ks) \[Theta]s^4-4 ks \[Theta]s^2 (\[Theta]^2+2 \[Theta]s^2) Log[\[Theta]/(2 \[Theta]s)])+(\[Theta]-\[Theta]s) (\[Theta]+\[Theta]s) (3 \[Theta]^3 \[Theta]s-2 (1+2 ks) \[Theta] \[Theta]s^3-4 \[Theta]^4 Sqrt[1-\[Theta]^2/\[Theta]s^2] Sqrt[1-(2 \[Theta])/(\[Theta]+\[Theta]s)]-2 (1+2 ks) \[Theta]s^4 (-1+Sqrt[1-\[Theta]^2/\[Theta]s^2] Sqrt[1-(2 \[Theta])/(\[Theta]+\[Theta]s)])+\[Theta]^2 \[Theta]s^2 (-3+4 Sqrt[1-\[Theta]^2/\[Theta]s^2] Sqrt[1-(2 \[Theta])/(\[Theta]+\[Theta]s)]+4 ks Sqrt[1-\[Theta]^2/\[Theta]s^2] Sqrt[1-(2 \[Theta])/(\[Theta]+\[Theta]s)])+4 (Sqrt[1-\[Theta]^2/\[Theta]s^2] (\[Theta]^2-\[Theta]s^2)^2 Sqrt[1-(2 \[Theta])/(\[Theta]+\[Theta]s)]+ks \[Theta]s^3 (-\[Theta]+\[Theta]s+2 Sqrt[1-\[Theta]^2/\[Theta]s^2] \[Theta]s Sqrt[1-(2 \[Theta])/(\[Theta]+\[Theta]s)])) Log[\[Theta]/(2 \[Theta]s)])))/(\[Theta]^3 Sqrt[1-\[Theta]^2/\[Theta]s^2] Sqrt[1-(2 \[Theta])/(\[Theta]+\[Theta]s)] (Sqrt[1-\[Theta]^2/\[Theta]s^2] (\[Theta]^2-(1+2 ks) \[Theta]s^2)+4 ks \[Theta]s^2 ArcTanh[Sqrt[1-(2 \[Theta])/(\[Theta]+\[Theta]s)]])^2)];
#
#
# (* ::Input:: *)
# (*(**)
# (*SlopeToPlot=D[Log[gNFWTangSphToPlot/.ks->0.2/.{\[Theta]->x,\[Theta]s->1}],x]/D[Log[x],x]//N;*)
# (*LogLinearPlot[SlopeToPlot,{x,10^-3,1},PlotRange->{-5,5}]*)
# (*\[Theta]ENFWFromks\[Theta]s[0.2,1]*)
# (**)*)
#
#
# (* ::Input:: *)
# (*(**)
# (*LogLogPlot[kNFWSphToPlot/.ks->1./.{\[Theta]->x,\[Theta]s->1},{x,10^-2,1},PlotRange->All]*)
# (**)*)
#
#
# (* ::Input:: *)
# (*(**)
# (*(*check kWhitin*)*)
# (*\[Theta]v=0.5;*)
# (*1/(\[Pi]*\[Theta]v^2) Integrate[2\[Pi]*\[Theta]*kNFWSph/.{ks->1,\[Theta]s->1},{\[Theta],0,\[Theta]v}]//Chop*)
# (*kNFWWhitin/.{ks->1,\[Theta]s->1}/.\[Theta]->\[Theta]v*)
# (**)*)
#
#
# (* ::Input:: *)
# (*(**)
# (*\[Gamma]NFWTangSph/ks/.\[Theta]->\[Theta]s*x/.x->{0.99,1.01}*)
# (*kNFWSph/kNFWRotTra/.{\[Theta]10->0,\[Theta]20->0}/.{\[Theta]sP->\[Theta]s,\[Theta]1->\[Theta],\[Theta]2->0,\[Epsilon]->0,\[Theta]\[Epsilon]->0}/.{ks->0.2,\[Theta]s->1,\[Theta]->0.8}//Simplify*)
# (*(kNFWWhitin-kNFWSph)/\[Gamma]NFWTangSph/.{ks->2,\[Theta]s->1,\[Theta]->0.9}//Simplify*)
# (**)*)
#
#
# (*
# ksMin=0.05;
# xENFWSphTab=Join[
# {{0.,0.}},
# Table[{ksToTab,x}/.FindRoot[1==kNFWWhitinToPlot/.{\[Theta]s->1.,\[Theta]->x,ks->ksToTab},{x,10^-4.}]//Chop,
# {ksToTab,ksMin,10^2,ksMin}]]//Chop;
# xENFWSphInter=Interpolation[xENFWSphTab];
# *)
# xENFWSphInter=Interpolation[ReadList["/Users/maurosereno/Library/Mathematica/Applications/data/tab_xENFWSph.dat",Real,RecordLists->True]];
#
# \[Theta]ENFWFromks\[Theta]s[ks_,\[Theta]s_]:=\[Theta]s*xENFWSphInter[ks];
#
#
# (* ::Subsection:: *)
# (*Gravitational potential*)
#
#
# (* ::Text:: *)
# (*Approximated potential of a NFW halo (Lee & Suto 2003, Eq. 27)*)
#
#
# F1NFW[u_]:=-(1/u)*Log[1+u];
# F2NFW[u_]:=-(1/3)+(2u^2-3u+6)/(6u^2)+(1/u-1/u^3)Log[1+u];
# F3NFW[u_]:=(u^2-3u-6)/(2u^2 (1+u))+3/u^3 Log[1+u];
# \[CapitalPhi]NFWLS03[\[Xi]_]:=4\[Pi] G \[Delta]c \[Rho]Cr*rs^2 (F1NFW[\[Xi]/rs]+(ec1^2+ec2^2)/2 F2NFW[\[Xi]/rs])/.{ec1->Sqrt[1-q1^2],ec2->Sqrt[1-q2^2]};
# d\[CapitalPhi]NFWLS03[\[Xi]_]:=(D[\[CapitalPhi]NFWLS03[x],x]/.x->\[Xi])//Simplify;
#
#
# (* ::Subsection:: *)
# (*Concentration*)
#
#
# (* ::Text:: *)
# (*Changing overdensity*)
#
#
# (* ::Subsubsection:: *)
# (*From \[CapitalDelta]_Vir to \[CapitalDelta]=200*)
#
#
# \[CapitalDelta]Cr=18.\[Pi]^2+82.d-39.d^2/.d->\[CapitalOmega]Mz-1.; (*flat universe, Eq. 6 Bryan & Norman 1998ApJ...495...80B*)
# \[CapitalDelta]CrKS=18.\[Pi]^2 (1.+0.4093*wf^0.9052)/.wf->1./\[CapitalOmega]Mz-1.;(*flat universe, Eq. A5 Kitayama & Suto, 1996ApJ...469..480K *)
# \[Delta]CrKS=(3.(12.\[Pi])^(2/3))/20. (1.+0.0123 Log10[\[CapitalOmega]Mz]); (*flat universe, Eq. A6 Kitayama & Suto, 1996ApJ...469..480K *)
#
# c\[CapitalDelta]TocVirSub={c\[CapitalDelta]->1/((a1 fc^(2*pcV)+(3/4)^2)^(-1/2)+2fc)/.pcV->a2+a3 Log[fc]+a4 Log[fc]^2/.{fc->\[CapitalDelta]/(cVir^3 \[CapitalDelta]Vir) (Log[1+cVir]-cVir/(1+cVir))}/.{a1->0.5116,a2->-0.4283,a3->-3.13*10^-3,a4->-3.52*10^-5}};
# M\[CapitalDelta]ToMVirSub={M\[CapitalDelta]->\[CapitalDelta]/\[CapitalDelta]Vir (c\[CapitalDelta]/cVir)^3 MVir};
# MVirToM\[CapitalDelta]Sub={MVir->\[CapitalDelta]Vir/\[CapitalDelta] (cVir/c\[CapitalDelta])^3 M\[CapitalDelta]};
#
#
# (*the units of the output mass are the same as the input mass*)
# fVirFrom\[CapitalDelta][\[CapitalDelta]v_,\[CapitalOmega]M0v_,zdCluster_]:=Module[{cVirMin,cVirMax,cVirStep,fc200FromcVir,fcVirFromc200Tmp1},
# {cVirMin,cVirMax,cVirStep}={0.05,100.,0.05};
# fc200FromcVir=Interpolation[Union[{{0.,0.}},Table[{cVir,c\[CapitalDelta]/.c\[CapitalDelta]TocVirSub/.{\[CapitalDelta]Vir->\[CapitalDelta]Cr,\[CapitalDelta]->\[CapitalDelta]v}/.{\[CapitalOmega]M0->\[CapitalOmega]M0v,z->zdCluster}},{cVir,cVirMin,cVirMax,cVirStep}]]];
#
# fcVirFromc200Tmp1[c\[CapitalDelta]_]:=FindRoot[c\[CapitalDelta]==fc200FromcVir[cVir],{cVir,c\[CapitalDelta]}][[1,2]];
# fcVirFromc200=Interpolation[Union[{{0.,0.}},Table[{c\[CapitalDelta],fcVirFromc200Tmp1[c\[CapitalDelta]]},{c\[CapitalDelta],cVirMin,fc200FromcVir[cVirMax],cVirStep}]]];
# fMVirFromM200[M200_,c200_]:=\[CapitalDelta]Vir/\[CapitalDelta] (cVir/c200)^3 M200/.{\[CapitalDelta]Vir->\[CapitalDelta]Cr,\[CapitalDelta]->\[CapitalDelta]v}/.{\[CapitalOmega]M0->\[CapitalOmega]M0v,z->zdCluster}/.cVir->fcVirFromc200[c200]
# ;
# ];
#
#
# (* ::Subsubsection:: *)
# (*Conversion*)
#
#
# (* ::Text:: *)
# (*From M and c to {k_s, r_s}*)
#
#
# r\[CapitalDelta]ToM\[CapitalDelta]SphSub=(Solve[M\[CapitalDelta]==(4\[Pi])/3 \[CapitalDelta] \[Rho]Cr r\[CapitalDelta]^3,r\[CapitalDelta]]//eSimpl//Flatten)[[2]];
# McTokrSubTmp1=Solve[{M\[CapitalDelta]==4\[Pi] \[CapitalSigma]Cr ks rs^2 (Log[1+c\[CapitalDelta]]-c\[CapitalDelta]/(1+c\[CapitalDelta])),c\[CapitalDelta]==r\[CapitalDelta]/rs}/.r\[CapitalDelta]ToM\[CapitalDelta]SphSub,{ks,rs}]//eSimpl//Flatten
# ksrsToMcSphSub={McTokrSubTmp1,\[CapitalDelta]ks->Sqrt[(D[ks/.McTokrSubTmp1,M\[CapitalDelta]]\[CapitalDelta]M\[CapitalDelta])^2+(D[ks/.McTokrSubTmp1,c\[CapitalDelta]]\[CapitalDelta]c\[CapitalDelta])^2],\[CapitalDelta]rs->Sqrt[(D[rs/.McTokrSubTmp1,M\[CapitalDelta]]\[CapitalDelta]M\[CapitalDelta])^2+(D[rs/.McTokrSubTmp1,c\[CapitalDelta]]\[CapitalDelta]c\[CapitalDelta])^2]}//Flatten;
# r\[CapitalDelta]ToM\[CapitalDelta]Sub=Solve[M\[CapitalDelta]==(4\[Pi])/3 (\[CapitalDelta] \[Rho]Cr)  r\[CapitalDelta]^3 q1 q2,r\[CapitalDelta]][[2]]
# r200ToM200Sub=r\[CapitalDelta]ToM\[CapitalDelta]Sub/.{r\[CapitalDelta]->r200,M\[CapitalDelta]->M200,\[CapitalDelta]->200};
# rsPTo3DParSub=rsP->rs (Sqrt[f]/e\[CapitalDelta])^-1/.rs->r200/c200/.r200ToM200Sub/.e\[CapitalDelta]ToFacGeoSub/.f->f\[Zeta]/.\[Theta]ToCos\[Theta]Sub/.{e1->1/q1,e2->1/q2,e3->1}/.{FacGeo->FacGeoCos\[Theta]\[CurlyPhi],eP->ePCos\[Theta]\[CurlyPhi]};
# M\[CapitalDelta]Tor\[CapitalDelta]Sub=Solve[M\[CapitalDelta]==(4\[Pi])/3 (\[CapitalDelta] \[Rho]Cr)  r\[CapitalDelta]^3 q1 q2,M\[CapitalDelta]]//Flatten
#
#
# (* ::Input:: *)
# (*(*r\[CapitalDelta]/(Mpc/h)/.r\[CapitalDelta]ToM\[CapitalDelta]Sub/.CritSub/.H->Hubble\[CapitalLambda][zdCluster,\[CapitalOmega]M0v,1-\[CapitalOmega]M0v]/.H0TohSub/.h->hv/.\[CapitalDelta]->\[CapitalDelta]Cr/.{\[CapitalOmega]M0->\[CapitalOmega]M0v,z->zdCluster}/.{q1->1,q2->1}/.M\[CapitalDelta]->{1.5,2.1}*10^15 MSun/.SubMKS/.SubMKS*)*)
#
#
# (* ::Text:: *)
# (*NFW parameters. Subscript[k, NFW](0)=1/Subscript[\[CapitalSigma], cr] 1/Subscript[e, \[CapitalDelta]] Subscript[\[Delta], c] Subscript[\[Rho], cr] Subscript[\[Theta], sP] Subscript[D, d], For a spherical halo Subscript[k, s]=Subscript[r, s] Subscript[\[Delta], c]Subscript[\[Rho], Cr]/Subscript[\[CapitalSigma], Cr] (WRIGHT & BRAINERD, 2000). *)
#
#
# \[Delta]c200Sub=\[Delta]c200->200/3 c200^3/(Log[1+c200]-c200/(1+c200));
# ksNFWTri=FacGeo/Sqrt[eP] (\[Delta]c200 \[Rho]Cr*rsP)/\[CapitalSigma]Cr/.\[Delta]c200Sub/.CritSub/.ePTo\[Epsilon]Sub;
# Tri200Sub={r200->(rsP rsFacGeo/Sqrt[eP])*c200,M200->200 (4\[Pi])/3 (c200^3 rsP^3)/eP^(3/2)*\[Rho]Cr*FacGeo,
# rs->rsP/Sqrt[eP] rsFacGeo,ks->ksNFWTri}/.ePTo\[Epsilon]Sub;
#
# rsNFWSph=rsP/.rsPTo3DParSub/.{FacGeo->1,eP->1,\[Epsilon]->0,q1->1,q2->1,Cos\[Theta]->0,\[CurlyPhi]->0};
# ksNFWSph=ksNFWTri/.rsP->rsNFWSph/.{FacGeo->1,eP->1,\[Epsilon]->0,q1->1,q2->1,Cos\[Theta]->0,\[CurlyPhi]->0};
#
#
# (* ::Text:: *)
# (*generalized \[Gamma]NFW*)
#
#
# (*
# Integrate form from Keeton (2002).
# The expression in terms of Hypergeometric has been derived integrating with mathematica*)
#
# fk\[Gamma]NFW=Function[{x,ks,\[Gamma]},
# Block[{k\[Gamma]NFWTmp1},
# 2.ks*If[x!=1 &&\[Gamma]<3,
# If[(k\[Gamma]NFWTmp1=(x^(1.-\[Gamma])((1.+x)^(\[Gamma]-3.)-(3.-\[Gamma])x^(-4+\[Gamma])*(x (-1.-3.*x-3.*x^2+(-1+(1.+1./x)^\[Gamma])*x^3) Gamma[3.-\[Gamma]]/Gamma[4.-\[Gamma]]/(1.+x)^3
# +1./(12.*x) (3.*\[Pi]*x*Hypergeometric2F1[2.-.5\[Gamma],2.5-.5*\[Gamma],2.,x^-2]+4.(\[Gamma]-4.) HypergeometricPFQ[{1.,2.5-0.5\[Gamma],3.-0.5\[Gamma]},{1.5,2.5},x^-2]))))//Chop)\[Element]Reals,
# k\[Gamma]NFWTmp1,
# x^(1.-\[Gamma]) ((1.+x)^(\[Gamma]-3.)+(3.-\[Gamma])NIntegrate[(y+x)^(\[Gamma]-4.)(1-Sqrt[1.-y^2]),{y,0.,1.}])
# ],
# x^(1.-\[Gamma]) ((1.+x)^(\[Gamma]-3.)+(3.-\[Gamma])NIntegrate[(y+x)^(\[Gamma]-4.)(1-Sqrt[1.-y^2]),{y,0.,1.}])
# ]
# ],Listable
# ];
#
# fk\[Gamma]NFWInteg=Function[{x,ks,\[Gamma]},2.*ks*x^(1.-\[Gamma])*((1.+x)^(\[Gamma]-3.)+(3.-\[Gamma])NIntegrate[(y+x)^(\[Gamma]-4.)(1-Sqrt[1.-y^2]),{y,0.,1.}]),Listable];
#
# k\[Gamma]NFWExpr=2ks x^(1-\[Gamma]) ((1+x)^(\[Gamma]-3)+(3-\[Gamma])x^(\[Gamma]-4) (
# Gamma[3-\[Gamma]]/Gamma[4-\[Gamma]] x (1-(x/(1+x))^(3-\[Gamma]))-(3 \[Pi])/12  Hypergeometric2F1[(5-\[Gamma])/2,2-\[Gamma]/2,2,1/x^2]+(4-\[Gamma])/(3x)  HypergeometricPFQ[{1,5/2-\[Gamma]/2,3-\[Gamma]/2},{3/2,5/2},1/x^2]
# ));
# (*
# Simplify[Integrate[(y+x)^(\[Gamma]-4) (1-Sqrt[1-y^2]),{y,0,1}],{0\[LessEqual] \[Gamma]\[LessEqual] 3,x>0}]
# *)
#
# (* interpolating function *)
# str=OpenRead["/Users/maurosereno/Documents/calcoli/data/table_k_gammaNFW.dat"];
# Skip[str,String,1];
# k\[Gamma]NFWTab=ReadList[str,Real,RecordLists->True];
# k\[Gamma]NFWInterTmp1=Interpolation[k\[Gamma]NFWTab,InterpolationOrder->1];
# fk\[Gamma]NFWInter=Compile[{{x,_Real},{ks,_Real},{\[Gamma],_Real}},ks*k\[Gamma]NFWInterTmp1[Log10[x],\[Gamma]],RuntimeAttributes->Listable,Parallelization->True,CompilationTarget->"C"];
#
# fk\[Gamma]NFWx2Inter=Compile[{{x1,_Real},{x2,_Real},{x01,_Real},{x02,_Real},{ks,_Real},{rsP,_Real},{\[Gamma],_Real},{\[Epsilon],_Real},{\[Theta]\[Epsilon],_Real}},
# Block[{x,Cos\[Theta]\[Epsilon],Sin\[Theta]\[Epsilon]},
# Cos\[Theta]\[Epsilon]=Cos[0.017453292519943295`*\[Theta]\[Epsilon]];
# Sin\[Theta]\[Epsilon]=Sin[0.017453292519943295`*\[Theta]\[Epsilon]];
# x=Sqrt[((x1-x01)*Sin\[Theta]\[Epsilon]-(x2-x02)*Cos\[Theta]\[Epsilon])^2+((x1-x01)*Cos\[Theta]\[Epsilon]+(x2-x02)*Sin\[Theta]\[Epsilon])^2/(1.-\[Epsilon])^2]/rsP;
# Return[fk\[Gamma]NFWInter[x,ks,\[Gamma]]]
# ],
# RuntimeAttributes->Listable,CompilationTarget->"C",RuntimeOptions->"Speed",Parallelization->True];
# Clear[str,k\[Gamma]NFWTab];
#
#
# (* ::Input:: *)
# (*(**)
# (*Log10xTab=Join[*)
# (*Table[Log10x,{Log10x,-8.,-4.,1.}],Table[Log10x,{Log10x,-3.99,-3.,.01}],Table[Log10x,{Log10x,-2.999,2.,.001}],Table[Log10x,{Log10x,2.01,4.,.01}],Table[Log10x,{Log10x,5.,8.,1.}]];*)
# (**)
# (*{\[Gamma]Min,\[Gamma]Max,\[CapitalDelta]\[Gamma]}={0.,3.,.05};*)
# (*\[Gamma]Tab=Table[\[Gamma],{\[Gamma],\[Gamma]Min,\[Gamma]Max,\[CapitalDelta]\[Gamma]}];*)
# (**)
# (*Timing[k\[Gamma]NFWTab=Reap[Do[*)
# (*Sow[{Log10xTab[[IndLog10x]],\[Gamma]Tab[[Ind\[Gamma]]],fk\[Gamma]NFWInteg[10.^Log10xTab[[IndLog10x]],1.,\[Gamma]Tab[[Ind\[Gamma]]]]//Chop}],*)
# (*{IndLog10x,1,Length[Log10xTab]},{Ind\[Gamma],Length[\[Gamma]Tab]}]*)
# (*][[2,1]];]*)
# (*Timing[Export["/Users/maurosereno/Documents/calcoli/data/table_k_gammaNFW.dat",Join[{{"# ","Log10x ","gamma ", "k "}},k\[Gamma]NFWTab]];]*)
# (*Clear[k\[Gamma]NFWTab,\[Gamma]Min,\[Gamma]Max,\[CapitalDelta]\[Gamma],Log10x,\[Gamma],Log10xTab,\[Gamma]Tab];*)
# (**)*)
#
#
# k\[Gamma]NFWRotTra=(*Re[*)(2ks x^(1-\[Gamma]) ((1+x)^(\[Gamma]-3)+(3-\[Gamma])x^(\[Gamma]-4) (
# Gamma[3-\[Gamma]]/Gamma[4-\[Gamma]] x (1-(x/(1+x))^(3-\[Gamma]))-(3 \[Pi])/12  Hypergeometric2F1[(5-\[Gamma])/2,2-\[Gamma]/2,2,1/x^2]+(4-\[Gamma])/(3x)  HypergeometricPFQ[{1,5/2-\[Gamma]/2,3-\[Gamma]/2},{3/2,5/2},1/x^2]
# )
# )/.x->\[Theta]Ell/\[Theta]sP/.\[Theta]EllSub/.\[Theta]RotSub)/.\[Theta]TraSub(*]*);
#
# (*
# 2ArcTanh[Sqrt[(1-x)/(1+x)]]*ArcCosh[1/x]^-1/.x->0.2
# *)
# (*
# kNFWRotTraKee/kNFWRotTra/.{\[Theta]10->0.1,\[Theta]20->0.1,\[Theta]sP->100,\[Epsilon]->0.4,\[Theta]\[Epsilon]->0}/.{\[Theta]1->10,\[Theta]2->30}
# *)
# (*
# NFWLogSlope=D[Log[2ks (1-FCorKee[x])/(x^2-1)],x]/D[Log[x],x]//Simplify
# *)
#
#
# (* ::Section:: *)
# (*Priors*)
#
#
# (* ::Subsection:: *)
# (*PDF*)
#
#
# (* ::Subsubsection:: *)
# (*Axial ratios*)
#
#
# (* ::Text:: *)
# (*p(q_1,q_2)dq_1dq_2*)
# (*=p(q_1)dq_1 p(q_2|q_1)dq_2*)
# (*=p(q_1)dq_1 p(q_2/q_1|q_1) d(q_2/q_1)*)
# (*=p(q_1)dq_1 p(q_1/q_2|q_1) d(q_1/q_2)*)
# (**)
# (*p(q_1/q_2|q_1)d (q_1/q_2)=p(q_2|q_1)(q_1/q_2^2) dq_2*)
#
#
# (* ::Text:: *)
# (*M_vir in "PqNBodyJS02" is in units of M_Sun/h*)
# (**)
# (*"PqFlatq1Cut": The probability is such that  P(q_ 1)=const and P(q_ 2|q_ 1)=const.  in the allowed region; *)
# (*p(q_1)=1/(1-q_min) *)
# (*p(q_2)=1/(1-q_min) Log[(1-q_min)/(1-q_2)]*)
# (*"PqFlatq1q2Cut": The probability is constant for each couple {q_ 1,q_ 2} in the allowed region*)
# (*p(q_ 1)=1/2 (1-q_min)^2 (1-q_ 1)*)
# (*p(q_ 1)=1/2 (1-q_min)^2 (q_ 2-q_min)*)
#
#
# zMStarJS02Tab={{0.,9.4*10^12},{0.5,2.0*10^12},{1.0,3.8*10^11}} (* M* in units of M_Sun/h*);
# zLog10MStarJS02Tab=Table[{zMStarJS02Tab[[i,1]],Log[10,zMStarJS02Tab[[i,2]]]},{i,1,Length[zMStarJS02Tab]}];
# fLog10MStarJS02Inter=Interpolation[zLog10MStarJS02Tab,InterpolationOrder->1];
#
# PqNBodyJS02[q1_,q2_,{MVir_,zCl_,\[CapitalOmega]M0_}]:=Block[
# {\[CapitalOmega]Mz,q1ScalONq1,rMin,Pq1ONq2},
# \[CapitalOmega]Mz=(\[CapitalOmega]M0 (1+zCl)^3)/(\[CapitalOmega]M0 (1+zCl)^3+(1-\[CapitalOmega]M0));
# q1ScalONq1=((MVir/10^fLog10MStarJS02Inter[zCl])^(0.07(\[CapitalOmega]Mz)^0.7));
# rMin=Max[q1,0.5];
# Pq1ONq2=3./(2.(1.-rMin)) (1-((2.(q1/q2)-1-rMin)/(1-rMin))^2);
# 1/(Sqrt[2\[Pi]](0.113/q1ScalONq1)) Exp[-((q1-0.54/q1ScalONq1)^2/(2.*(0.113/q1ScalONq1)^2))]*If[q1/q2>rMin,(q1/q2^2)Pq1ONq2,0]
# ];
#
#
# fPqNBodyJS02[q1_,q2_,MVir_(*MSun/h*),zCl_,\[CapitalOmega]M0_]:=
# Block[
# {\[CapitalOmega]Mz,q1ScalONq1,rMin,Pq1ONq2,Pq2Iq1},
# \[CapitalOmega]Mz=(\[CapitalOmega]M0 (1.+zCl)^3)/(\[CapitalOmega]M0 (1.+zCl)^3+(1.-\[CapitalOmega]M0));
# q1ScalONq1=((MVir/10^fLog10MStarJS02Inter[zCl])^(0.07(\[CapitalOmega]Mz)^0.7));
# rMin=Max[q1,0.5];
# If[q1/q2>rMin,
# Pq1ONq2=3./(2.(1.-rMin)) (1-((2.(q1/q2)-1-rMin)/(1-rMin))^2);
# Pq2Iq1=(q1/q2^2)Pq1ONq2,
# Pq2Iq1=0.];
# Return[1./(Sqrt[2\[Pi]](0.113/q1ScalONq1)) Exp[-((q1-0.54/q1ScalONq1)^2/(2.*(0.113/q1ScalONq1)^2))]*Pq2Iq1]
# ];
#
# Pq1NBody[q1_,{MVir_,zCl_,\[CapitalOmega]M0_}]:=Module[
# {\[CapitalOmega]Mz,q1ScalONq1},
# \[CapitalOmega]Mz=(\[CapitalOmega]M0 (1.+zCl)^3)/(\[CapitalOmega]M0 (1.+zCl)^3+(1.-\[CapitalOmega]M0));
# q1ScalONq1=((MVir/10^fLog10MStarJS02Inter[zCl])^(0.07(\[CapitalOmega]Mz)^0.7));
# 1/(Sqrt[2\[Pi]](0.113/q1ScalONq1)) Exp[-((q1-0.54/q1ScalONq1)^2/(2*(0.113/q1ScalONq1)^2))]
# ];
#
#
# fqFlatq1CutRandom[{q1Min_}]:=Module[{q1Tmp1,q2Tmp1},
# q1Tmp1=RandomReal[{q1Min,1}];
# q2Tmp1=RandomReal[{q1Tmp1,1}];
# {q1Tmp1,q2Tmp1}];
#
# fqFlatq2CutRandom[{q2Min_,q1Onq2Min_}]:=Module[{q1Tmp1,q2Tmp1},
# q2Tmp1=RandomReal[{q2Min,1}];
# q1Tmp1=q2Tmp1*RandomReal[{q1Onq2Min,1}];
# {q1Tmp1,q2Tmp1}];
#
#
# Pq1ONq2[q1ONq2_,q1_]:=Module[{rMin,Pq1ONq2Tmp1},
# rMin=Max[q1,1/2];
# Pq1ONq2Tmp1=3/(2(1-rMin)) (1-((2q1ONq2-1-rMin)/(1-rMin))^2);
# If[q1ONq2>rMin,Pq1ONq2Tmp1,0]
# ];
# q1ONq2Max[q1_]:=1/2 (1+Max[1/2,q1])//N;
#
# fqNBodyRandom[{MVir_,zClv_,\[CapitalOmega]M0v_}]:=Module[
# {q1Tmp1,q2Tmp1,q1ScalONq1,q1ONq2MaxTmp1,iTmp1,iTmp2,q1ONq2TryTmp1},
# q1ScalONq1=((MVir/MStar)^(0.07(\[CapitalOmega]Mz)^0.7))/.{MStar->10^fLog10MStarJS02Inter[z]}/.z->zClv/.\[CapitalOmega]M0->\[CapitalOmega]M0v;
# q1Tmp1=RandomReal[NormalDistribution[ 0.54/q1ScalONq1,0.113/q1ScalONq1]];
# q1ONq2MaxTmp1=q1ONq2Max[q1Tmp1];
# For[
# iTmp1=iTmp2=0,iTmp1<1,iTmp2++,q1ONq2TryTmp1=RandomReal[{Max[q1Tmp1,1/2],1}];If[Pq1ONq2[q1ONq2MaxTmp1,q1Tmp1]*RandomReal[]<Pq1ONq2[q1ONq2TryTmp1,q1Tmp1],q2Tmp1=q1Tmp1/q1ONq2TryTmp1;iTmp1++]
# ];
# {q1Tmp1,q2Tmp1}];
#
# fqRandomJS02[MVir_,zCl_,\[CapitalOmega]M0_]:=Block[
# {q1Tmp1,q2Tmp1,q1ScalONq1,q1ONq2MaxTmp1,iTmp1,iTmp2,q1ONq2TryTmp1,\[CapitalOmega]Mz},
# \[CapitalOmega]Mz=(\[CapitalOmega]M0 (1.+zCl)^3)/(\[CapitalOmega]M0 (1.+zCl)^3+(1.-\[CapitalOmega]M0));
# q1ScalONq1=((MVir/10^fLog10MStarJS02Inter[zCl])^(0.07(\[CapitalOmega]Mz)^0.7));
# q1Tmp1=RandomReal[NormalDistribution[ 0.54/q1ScalONq1,0.113/q1ScalONq1]];q1ONq2MaxTmp1=q1ONq2Max[q1Tmp1];
# For[
# iTmp1=iTmp2=0,iTmp1<1,iTmp2++,q1ONq2TryTmp1=RandomReal[{Max[q1Tmp1,1/2],1}];
# If[Pq1ONq2[q1ONq2MaxTmp1,q1Tmp1]*RandomReal[]<Pq1ONq2[q1ONq2TryTmp1,q1Tmp1],q2Tmp1=q1Tmp1/q1ONq2TryTmp1;
# iTmp1++]
# ];
# {q1Tmp1,q2Tmp1}];
#
# PqFlatq1Cut[q1_,q2_,q1Min_]:=If[q1Min<=q1<=q2<=1,1/(1-q1Min)*1/(1-q1),0];
# Pq1Flatq1Cut[q1_,q1Min_]:=1/(1-q1Min);
# Pq2Flatq1Cut[q2_,q1Min_]:=Log[(1-q1Min)/(1-q2)]/(1-q1Min);
# PqFlatq1q2Cut[q1_,q2_,q1Min_]:=If[q1Min<=q1<=q2<=1,1/2 (1-q1Min)^2,0];
# Pq2Iq1Flatq1Cut[q2_,q1_]:=1/(1-q1);
#
#
# (* ::Subsubsection:: *)
# (*Orientation*)
#
#
# (* ::Text:: *)
# (*\[Theta] and \[CurlyPhi]*)
#
#
# PCos\[Theta]Flat[Cos\[Theta]_]:=If[0<=Cos\[Theta]<=1,1,0];
# PCos\[Theta]Bias[Cos\[Theta]_,\[Sigma]Cos\[Theta]_]:=If[0<= Cos\[Theta]<=1,2/(Sqrt[2\[Pi]]\[Sigma]Cos\[Theta]) Exp[-(1/2) (1-Cos\[Theta])^2/\[Sigma]Cos\[Theta]^2],0];
#
# PCos\[CurlyPhi]Flat[Cos\[CurlyPhi]_]:=If[0<=Cos\[CurlyPhi]<=1,(\[Pi]/2)^-1 1/Sin[ArcCos[Cos\[CurlyPhi]]],0];
# P\[CurlyPhi]Flat[\[CurlyPhi]_]:=(\[Pi])^-1 If[0<=\[CurlyPhi]<=\[Pi],1,0];
# P\[Psi]Flat[\[Psi]_]:=(\[Pi])^-1 If[0<=\[Psi]<=\[Pi],1,0];
#
# fCos\[Theta]FlatRandom[{}]:=RandomReal[{0,1}];
#
# fCos\[Theta]BiasRandom[{\[Sigma]Cos\[Theta]_}]:=Module[{Cos\[Theta]Tmp1,iTmp1,iTmp2},
# For[iTmp1=iTmp2=0,iTmp1<1,iTmp2++,Cos\[Theta]Tmp1=RandomReal[NormalDistribution[1,\[Sigma]Cos\[Theta]]];If[0<=Cos\[Theta]Tmp1<=2,iTmp1++]];
# If[Cos\[Theta]Tmp1<=1,Cos\[Theta]Tmp1,2-Cos\[Theta]Tmp1]
# ];
#
#
# (* ::Subsubsection:: *)
# (*Checks*)
#
#
# (* ::Input:: *)
# (*(*Integrate[1/(1-q1Min)*1/(1-q1),{q2,q1,1}]*)*)
#
#
# (* ::Input:: *)
# (*(**)
# (*Integrate[1/(1-q1Min)*1/(1-q1),{q2,q1,1}]*)
# (*Simplify[Integrate[1/(1-q1Min)*1/(1-q1),{q1,q1Min,q2}],{0<q1Min<q2<1}]*)
# (**)*)
#
#
# (* ::Input:: *)
# (*(*Integrate[1,{q1,q1Min,1},{q2,q1,1}]//Simplify*)*)
#
#
# (* ::Input:: *)
# (*(**)
# (*Integrate[1/2 (1-q1Min)^2,{q2,q1,1}]//Simplify*)
# (*Simplify[Integrate[1/2 (1-q1Min)^2,{q1,q1Min,q2}],{0<q1Min<q2<1}]*)
# (**)*)
#
#
# (* ::Input:: *)
# (*(**)
# (*q1Minv=0.3;*)
# (*ContourPlot[{q1==q2,q1==q1Minv},{q1,0,1},{q2,0,1},FrameLabel->{"Subscript[q, 1]","Subscript[q, 2]"}]*)
# (**)*)
#
#
# (* ::Input:: *)
# (*(**)
# (*Show[*)
# (*Histogram[Table[Cos[RandomReal[{0,\[Pi]/2}]],{i,10^4}],Automatic,"ProbabilityDensity"],*)
# (*Plot[(\[Pi]/2)^-1 1/Sin[ArcCos[Cos\[CurlyPhi]]],{Cos\[CurlyPhi],0,1},PlotRange->{0,5}]*)
# (*]*)
# (**)*)
#
#
# (* ::Section:: *)
# (*c-M relation*)
#
#
# (* ::Subsubsection:: *)
# (*Prada et al.*)
#
#
# cMinPrada[x_]:=c0+(c1-c0)(1/\[Pi] ArcTan[\[Alpha](x-x0)]+1./2.)/.{c0->3.681,c1->5.033,\[Alpha]->6.948,x0->0.424};
# \[Sigma]MinInvPrada[x_]:=\[Sigma]0^-1+(\[Sigma]1^-1-\[Sigma]0^-1)(1/\[Pi] ArcTan[\[Beta](x-x1)]+1./2.)/.{\[Sigma]0->1/1.047,\[Sigma]1->1/1.646,\[Beta]->7.386,x1->0.526};
# DGrowthRate[x_,\[CapitalOmega]M0_,\[CapitalOmega]\[CapitalLambda]0_]:=5./2. (\[CapitalOmega]M0/\[CapitalOmega]\[CapitalLambda]0)^(1/3) Sqrt[1+1/x^3]*NIntegrate[(y/(1+y^3))^(3/2),{y,0,x}];
# cPrada[M_,z_,h_,\[CapitalOmega]M0_,\[CapitalOmega]\[CapitalLambda]0_]:=Module[{a=(1.+z)^-1,x,\[Sigma]Mz,B0,\[Sigma]p,Cal},
# x=(\[CapitalOmega]\[CapitalLambda]0/\[CapitalOmega]M0)^(1./3.) a;
# \[Sigma]Mz=DGrowthRate[x,\[CapitalOmega]M0,\[CapitalOmega]\[CapitalLambda]0]/DGrowthRate[(\[CapitalOmega]\[CapitalLambda]0/\[CapitalOmega]M0)^(1/3),\[CapitalOmega]M0,\[CapitalOmega]\[CapitalLambda]0] ( 16.9y^0.41)/(1.+1.102y^0.20+6.22y^0.333)/.y->(M/(10^12.*h^-1*MSun))^-1/.SubMKS/.SubMKS;
# \[Sigma]p=B1*\[Sigma]Mz/.B1->\[Sigma]MinInvPrada[x]/\[Sigma]MinInvPrada[1.393];
# Cal=ACal((\[Sigma]p/bCal)^cCal+1.)Exp[dCal/\[Sigma]p^2]/.{ACal->2.881,bCal->1.257,cCal->1.022,dCal->0.060};
# Return[B0*Cal/.B0->cMinPrada[x]/cMinPrada[1.393]]
# ];
#
#
# (* ::Text:: *)
# (*Subscript[\[Sigma], Subscript[log, 10]c  ]=1/ln(10) Subscript[\[Sigma], ln c  ]*)
#
#
# (* ::Subsubsection:: *)
# (*Bhattacharya et al. 2013*)
#
#
# Options[cMBhattacharya13]={Sample->"Full"(*"Relaxed"*),\[CapitalDelta]->"200"(*"Vir"*)};
# cMBhattacharya13[M_(*in units of 10^14 MSun/h*),z_,\[CapitalOmega]M0_,\[CapitalOmega]\[CapitalLambda]0_,OptionsPattern[]]:=
# Block[{\[Nu],Dz,\[Alpha]D,\[Alpha]\[Nu],\[Beta],OptionValueSample,OptionValue\[CapitalDelta]},
# OptionValueSample=OptionValue[Sample];
# OptionValue\[CapitalDelta]=OptionValue[\[CapitalDelta]];
#
# Which[
# OptionValue\[CapitalDelta]=="200" &&OptionValueSample=="Full" ,
# \[Alpha]D=0.54;\[Alpha]\[Nu]=-0.35;\[Beta]=5.9,
# OptionValue\[CapitalDelta]=="200" &&OptionValueSample=="Relaxed" ,
# \[Alpha]D=0.53;\[Alpha]\[Nu]=-0.41;\[Beta]=6.6,
# OptionValue\[CapitalDelta]=="Vir" &&OptionValueSample=="Full" ,
# \[Alpha]D=0.9;\[Alpha]\[Nu]=-0.29;\[Beta]=7.7,
# OptionValue\[CapitalDelta]=="Vir" &&OptionValueSample=="Relaxed" ,
# \[Alpha]D=1.01;\[Alpha]\[Nu]=-0.34;\[Beta]=8.9
# ];
# Dz=D1\[CapitalLambda]Approx[z,\[CapitalOmega]M0,\[CapitalOmega]\[CapitalLambda]0]/D1\[CapitalLambda]Approx[0.,\[CapitalOmega]M0,\[CapitalOmega]\[CapitalLambda]0];
# \[Nu]=1./Dz (1.12(M/0.5)^0.3+0.53);
# Dz^\[Alpha]D \[Beta]*\[Nu]^\[Alpha]\[Nu]
# ];
#
#
# (* ::Subsubsection:: *)
# (*Bolshoi*)
#
#
# (* ::Input:: *)
# (*(**)
# (*TabTmp1=Table[{Log10i,c200Prior/.MultiDarkz0PriorSub/.M200->10^Log10i MSun/h/.SubMKS/.SubMKS,c200Prior/.DuffyFullPriorSub/.z->0./.M200->10^Log10i MSun/h/.SubMKS/.SubMKS},{Log10i,12,15,.1}];*)
# (*ListPlot[{TabTmp1[[All,{1,2}]],TabTmp1[[All,{1,3}]]},Joined->True,PlotStyle->{Red,Blue},Frame->True,Axes->False]*)
# (**)*)
#
#
# (* ::Input:: *)
# (*(**)
# (*Log[10,cPrada[10^13.8 MSun/hv,2.,hv,0.27,0.73]]*)
# (**)*)
#
#
# (* ::Input:: *)
# (*(**)
# (*zv=0.;*)
# (*TabTmp1=Table[{Log10i,c200Prior/.MultiDarkz0PriorSub/.M200->10^Log10i MSun/h/.SubMKS/.SubMKS,cPrada[10^Log10i MSun/hv,zv,hv,\[CapitalOmega]M0v,1-\[CapitalOmega]M0v]},{Log10i,12,15,.1}];*)
# (*ListPlot[{TabTmp1[[All,{1,2}]],TabTmp1[[All,{1,3}]]},Joined->True,PlotStyle->{Red,Blue},Frame->True,Axes->False]*)
# (**)*)
#
#
# (* ::Input:: *)
# (*(**)
# (*zv=.5;TabTmp1=Table[{Log10i,c200Bolshoi[10^Log10i MSun/hv,zv,hv,\[CapitalOmega]M0v][[1]],cPrada[10^Log10i MSun/hv,zv,hv,\[CapitalOmega]M0v,1-\[CapitalOmega]M0v]},{Log10i,12,15,.1}];*)
# (*ListPlot[{TabTmp1[[All,{1,2}]],TabTmp1[[All,{1,3}]]},Joined->True,PlotStyle->{Red,Blue}]*)
# (**)*)
#
#
# c200Prior=A (1+z)^Cd (M200/MPivot)^B;
# MultiDarkz0PriorSub={Posteriorv03`Par\[Sigma]->Log[10]*0.15,A->7.28,B->-0.074,Cd->0.,MPivot->10^12. MSun/h};
#
# DuffyFullPriorSub={Posteriorv03`Par\[Sigma]->Log[10]*0.15,A->5.71,\[CapitalDelta]A->0.12,B->-0.084,\[CapitalDelta]B->0.006,Cd->-0.47,\[CapitalDelta]C->0.04,MPivot->2*10^12 MSun/h};
# DuffyRelaxPriorSub={Posteriorv03`Par\[Sigma]->Log[10]*0.11,A->6.71,\[CapitalDelta]A->0.12,B->-0.091,\[CapitalDelta]B->0.009,Cd->-0.44,\[CapitalDelta]C->0.05,MPivot->2*10^12 MSun/h};
# NetoUnrelaxPriorSub={Posteriorv03`Par\[Sigma]->Log[10]*0.15,A->4.67,\[CapitalDelta]A->0.12,B->-0.11,\[CapitalDelta]B->0.009,Cd->-0.44,\[CapitalDelta]C->0.05,MPivot->10^14 MSun/h};
#
# cMFullPriorLog=Posterior`GaussPrior/.{Posterior`Par->Log[c200],Posterior`Par\[Mu]->(Log[c200Prior]//PowerExpand)(*Par\[Sigma]->Sqrt[(\[CapitalDelta]A/A)^2+(\[CapitalDelta]B*Log[M200/MPivot])^2+(\[CapitalDelta]C*Log[1+z])^2]*)}/.DuffyFullPriorSub/.h->hv/.z->zdCluster;
# cMFullPrior=1/c200 cMFullPriorLog;
#
# cMRelaxPriorLog=Posterior`GaussPrior/.{Posterior`Par->Log[c200],Posterior`Par\[Mu]->(Log[c200Prior]//PowerExpand)(*Par\[Sigma]->Sqrt[(\[CapitalDelta]A/A)^2+(\[CapitalDelta]B*Log[M200/MPivot])^2+(\[CapitalDelta]C*Log[1+z])^2]*)}/.DuffyRelaxPriorSub/.h->hv/.z->zdCluster;
# cMRelaxPrior=1/c200 cMRelaxPriorLog;
#
#
# (* ::Input:: *)
# (*(* NIntegrate[1/c200 cMPriorLog/.M200->10^15 MSun,{c200,0,20}] *)*)
# (*(* Plot[cMPrior/.M200->10^15 MSun,{c200,0,10},PlotRange->All,PlotStyle->{Black,Red}] *)*)
#
#
# (* ::Text:: *)
# (*Klypin et al. 2011*)
#
#
# cVirPrior=A (1+z)^Cd (MVir/MPivot)^B;
# Bolshoiz0PriorSub={Posteriorv03`Par\[Sigma]->Log[10]*0.15,A->9.60,B->-0.075,Cd->0.,MPivot->10^12 MSun/h} (*Klypin et al. 2011*);
#
# c0zTab={{0.,9.60},{0.5,7.08},{1.0,5.45},{2.0,3.67},{3.0,2.83},{5.0,2.34}};
# M0zTab={{0.,10^200.},{0.5,1.5*10^17},{1.0,2.5*10^15},{2.0,6.8*10^13},{3.0,6.3*10^12},{5.0,6.6*10^11}};
#
# c0zInter=Interpolation[c0zTab];
# M0zInter=Interpolation[M0zTab];
#
# cVirBolshoi[MVir_,z_,h_]:=c0zInter[z](MVir/(10^12 MSun/h))^-0.075 (1+(MVir/(M0zInter[z]*MSun/h))^0.26);
# c200Bolshoi[M200_,zv_,hv_,\[CapitalOmega]M0v_]:=Block[{\[CapitalDelta]CrBolshoi,cVirM200Tmp1,c\[CapitalDelta]Tmp1,MVirTmp1,cVirTmp1},
# (*\[CapitalDelta]CrBolshoi=(18.\[Pi]^2+82.d-39.d^2)/1.(*\[CapitalOmega]Mz*)/.d->\[CapitalOmega]Mz-1;*)
# {cVirM200Tmp1,c\[CapitalDelta]Tmp1,MVirTmp1}={cVirBolshoi[MVir,zv,hv],c\[CapitalDelta],MVir}/.MVirToM\[CapitalDelta]Sub/.c\[CapitalDelta]TocVirSub/.{\[CapitalDelta]->200.,\[CapitalDelta]Vir->\[CapitalDelta]Cr}/.M\[CapitalDelta]->M200/.{\[CapitalOmega]M0->\[CapitalOmega]M0v,z->zv}/.h->hv/.SubMKS/.SubMKS//N;
# cVirTmp1=cVir/.FindRoot[cVirM200Tmp1==cVir,{cVir,5.}];
# {c\[CapitalDelta]Tmp2,MVirTmp2}={c\[CapitalDelta]Tmp1,MVirTmp1}/.cVir->cVirTmp1;
# {c\[CapitalDelta]Tmp2,{cVirTmp1,MVirTmp2}}
# ];
#
#
# (* ::Subsubsection:: *)
# (*Maximum likelihood*)
#
#
# c200PDF=1/c200 Posterior`GaussPrior/.{Posterior`Par->Log[c200],Posterior`Par\[Mu]->Logc200Prior};
#
# c200PriorMaxSub=Solve[0==D[c200PDF,c200],c200]//Flatten;
# c200PDFNorm=Simplify[c200PDF/(c200PDF/.c200PriorMaxSub),{Posteriorv03`Par\[Sigma]>0,Posterior`Par\[Mu]>0,LogPDFv>0,Logc200Prior>0}];
#
# Logc200PriorCLSub=Simplify[Solve[(Log[c200PDFNorm/.c200->E^Logc200]//PowerExpand)==LogCLv,Logc200],
# {Posteriorv03`Par\[Sigma]>0,Posterior`Par\[Mu]>0,LogCLv<0}]
# c200PriorCLSub=c200/.c200->E^Logc200/.Logc200PriorCLSub
#
#
# (* ::Subsubsection:: *)
# (*Checks*)
#
#
# (* ::Input:: *)
# (*(**)
# (*{zdv,hv}={0.18,0.7}*)
# (*Plot[c200PDF/.c200PriorMaxSub/.Logc200Prior->Log[c200Prior]/.DuffyFullPriorSub/.M200->10^14 MSun*M200/.SubMKS/.{h->hv,z->zdv},{M200,10,20}]*)
# (**)*)
#
#
# (* ::Input:: *)
# (*(**)
# (*cMPriorTmp1=cMFullPrior/.M200->10^14 MSun*M200/.SubMKS/.{h->hv,z->zdv};*)
# (*cMFPriorLogTmp1=cMFullPriorLog/.M200->10^14 MSun*M200/.SubMKS/.{h->hv,z->zdv};*)
# (*c200PriorTmp1=c200Prior/.DuffyFullPriorSub/.M200->10^14 MSun*M200/.SubMKS/.{h->hv,z->zdv};*)
# (**)*)
#
#
# (* ::Input:: *)
# (*(**)
# (*M200v=10;*)
# (*cMPriorTmp2=cMPriorTmp1/.M200->M200v;*)
# (**)
# (*Plot[cMPriorTmp2,{c200,0,10}]*)
# (*FindMaximum[{cMPriorTmp2,c200>0},c200]*)
# (*c200MaxTmp1=c200/.c200PriorMaxSub/.Logc200Prior->Log[c200PriorTmp1]/.DuffyFullPriorSub/.M200->M200v*)
# (*PDFMaxTmp1=cMPriorTmp2/.c200->c200MaxTmp1*)
# (**)
# (**)
# (*c200PriorTmp1/.M200->M200v*)
# (**)
# (*{FindRoot[cMPriorTmp2/PDFMaxTmp1==Exp[-2.3/2],{c200,c200MaxTmp1-1}],FindRoot[cMPriorTmp2/PDFMaxTmp1==Exp[-2.3/2],{c200,c200MaxTmp1+1}]}*)
# (*c200PriorCLSub/.Logc200Prior->Log[c200PriorTmp1]/.DuffyFullPriorSub/.M200->M200v/.LogCLv->-2.3/2*)
# (**)
# (*Clear[M200v]*)
# (**)*)
#
#
# (* ::Input:: *)
# (*(**)
# (*Simplify[Integrate[cMFullPrior/.M200->M200*10^14 MSun/.SubMKS,{c200,0,\[Infinity]}],M200>0]*)
# (**)*)
#
#
# (* ::Input:: *)
# (*(**)
# (*c2000PriorTabTmp1=Table[Exp[RandomReal[NormalDistribution[Log[c200Prior]/.M200->20*10^14 MSun/.DuffyFullPriorSub/.h->hv/.z->zdCluster,Log[10]0.15]]],{i,10^4}];*)
# (**)*)
#
#
# (* ::Input:: *)
# (*(**)
# (*Show[*)
# (*Histogram[c2000PriorTabTmp1,Automatic,"ProbabilityDensity"],*)
# (*Plot[cMFullPrior/.M200->20*10^14 MSun/.SubMKS,{c200,0,12},PlotStyle->Black]*)
# (*(*,*)
# (*Plot[c200PDF/.Logc200Prior->Log[c200Prior]/.DuffyFullPriorSub/.h->hv/.z->zdv/.M200->20*10^14 MSun/.SubMKS,{c200,0,12},PlotStyle->Directive[Black,Thickness[0.005]]]*)
# (**)*)
# (*]*)
# (**)*)
#
#
# (* ::Section:: *)
# (*Cluster observed parameters*)
#
#
# (* ::Text:: *)
# (*ICM and lensing observale parameters*)
#
#
# (*ICM and lensing observale parameters*)
# ParObsAllTab={\[Theta]10,\[Theta]20,ks,\[Theta]sP,\[Gamma],\[Epsilon],\[Theta]\[Epsilon],\[Epsilon]ICM,\[Theta]\[Epsilon]ICM,e\[CapitalDelta]ICM,kSheet};
# ParObsAllTagTab={"\!\(\*SubscriptBox[\(\[Theta]\), \(1, 0\)]\)","\!\(\*SubscriptBox[\(\[Theta]\), \(2, 0\)]\)",
# "\!\(\*SubscriptBox[\(k\), \(s\)]\)","\!\(\*SubscriptBox[\(\[Theta]\), \(sP\)]\)","\[Gamma]",
# "\[Epsilon]","\!\(\*SubscriptBox[\(\[Theta]\), \(\[Epsilon]\)]\)",
# "\!\(\*SubscriptBox[\(\[Epsilon]\), \(ICM\)]\)","\!\(\*SuperscriptBox[SubscriptBox[\(\[Theta]\), \(\[Epsilon]\)], \(ICM\)]\)",
# "\!\(\*SuperscriptBox[SubscriptBox[\(e\), \(\[CapitalDelta]\)], \(ICM\)]\)","\!\(\*SubscriptBox[\(k\), \(Sheet\)]\)"};
# ParObsAllMinTab={{-10.,-10.},{0.,0.,0.},{0,-90.},{0.,-90.,0.},-10}//Flatten;
# ParObsAllMaxTab={{10.,10.},{10.,10.,2.},{1., 90.},{1.,90.,10^3.},10}//Flatten;
#
# (*lensing observale parameters*)
#
# (*{\[Theta]10,\[Theta]20,ks,\[Theta]sP,\[Epsilon],\[Theta]\[Epsilon],kSheet} indexes*)
# ParObsLensingIndTab={1,2,3,4,5,6,7,11};
# ParObsLensingTab=ParObsAllTab[[ParObsLensingIndTab]];
# ParObsLensingTagTab=ParObsAllTagTab[[ParObsLensingIndTab]];
# nParObsLensing=Length[ParObsLensingTab];
#
# (*{\[Theta]10,\[Theta]20,ks,\[Theta]sP,\[Epsilon],\[Theta]\[Epsilon]} indexes*)
# ParObsLensingToDeprIndTab={3,4,6,7};
# nParObsLensingToDepr=Length[ParObsLensingToDeprIndTab];
#
# ParObsLensingToDeprTab=ParObsAllTab[[ParObsLensingToDeprIndTab]];
#
#
# (* ::Subsubtitle:: *)
# (*preliminary*)
#
#
# (* ::Input:: *)
# (*(**)
# (*nbTmp1=NotebookOpen["/Users/maurosereno/Documents/calcoli/statistical_estimators_v10.nb"];*)
# (*SelectionMove[nbTmp1,All,Notebook]*)
# (*SelectionEvaluate[nbTmp1]*)
# (*NotebookClose[nbTmp1]*)
# (*Clear[nbTmp1];*)
# (**)*)
#
#
# (* ::Input:: *)
# (*(**)
# (*nb=NotebookOpen["/Users/maurosereno/Documents/calcoli/3DStructure/clusters_data.nb"];*)
# (*SelectionMove[nb,Next,CellGroupData[{Cell["A1689","Section"]}]]*)
# (*SelectionEvaluate[nb,Cell["A1689","Section"]]*)
# (*NotebookClose[nb]*)
# (*Clear[nb]*)
# (**)*)
#
#
# (* ::Section:: *)
# (*Cluster data*)
#
#
# (* ::Text:: *)
# (*Evaluate a section in "cluster_data.nb"*)
#
#
# (*NotebookOpen["/Users/maurosereno/Documents/calcoli/3DStructure/clusters_data.nb"];*)
#
#
# (* ::Input:: *)
# (*(**)
# (*SelectionMove[nb,Next,CellGroup[CellGroupData[{TextCell["heading","A1689"]}]]]*)
# (*NotebookSelection[nb]*)
# (*NotebookClose[nb]*)
# (*Clear[nb]*)
# (**)*)
#
#
# (* ::Title:: *)
# (*Private*)
#
#
# Begin["`Private`"]
#
#
# (* ::Subsection:: *)
# (*Cosmology*)
#
#
# (* ::Subsubsection:: *)
# (*\[CapitalLambda]CDM*)
#
#
# Hubble\[CapitalLambda][z_,\[CapitalOmega]M_,\[CapitalOmega]\[CapitalLambda]_]:=H0 Sqrt[\[CapitalOmega]M (1+z)^3+\[CapitalOmega]\[CapitalLambda]+(1-\[CapitalOmega]M-\[CapitalOmega]\[CapitalLambda])(1+z)^2];
# (*Hubble\[CapitalLambda]Comp=Compile[{{z,_Real},{\[CapitalOmega]M,_Real},{\[CapitalOmega]\[CapitalLambda],_Real}},H0 Sqrt[\[CapitalOmega]M (1+z)^3+\[CapitalOmega]\[CapitalLambda]+(1-\[CapitalOmega]M-\[CapitalOmega]\[CapitalLambda])(1+z)^2]];*)
#
#
# (*
# Hubble\[CapitalLambda][z_,\[CapitalOmega]_]:=H0 Sqrt[\[CapitalOmega] (1+z)^3+(1-\[CapitalOmega])];
# r\[CapitalLambda][x_,y_,\[CapitalOmega]_]:=1/(1+y) NIntegrate[H0/Hubble\[CapitalLambda][z,\[CapitalOmega]],{z,x,y}];
# *)
#
#
# (* ::Text:: *)
# (*Angular diameter distances*)
#
#
# Dist\[CapitalLambda][zd_,zs_,\[CapitalOmega]M_,\[CapitalOmega]\[CapitalLambda]_]:=  Block[{\[CapitalOmega]Kv},
# \[CapitalOmega]Kv=(1.-\[CapitalOmega]M-\[CapitalOmega]\[CapitalLambda]);
# 1./(1.+zs) Which[
# \[CapitalOmega]Kv <0,1./Sqrt[-\[CapitalOmega]Kv] Sin[ NIntegrate[(Sqrt[-\[CapitalOmega]Kv]H0)/Hubble\[CapitalLambda][z,\[CapitalOmega]M,\[CapitalOmega]\[CapitalLambda]],{z,zd,zs}]],
# \[CapitalOmega]Kv >0,1./Sqrt[\[CapitalOmega]Kv] Sinh[ NIntegrate[(Sqrt[\[CapitalOmega]Kv]H0)/Hubble\[CapitalLambda][z,\[CapitalOmega]M,\[CapitalOmega]\[CapitalLambda]],{z,zd,zs}]  ],
# \[CapitalOmega]Kv ==0,NIntegrate[H0/Hubble\[CapitalLambda][z,\[CapitalOmega]M,\[CapitalOmega]\[CapitalLambda]],{z,zd,zs}]   ]
# ];
#
# Dist\[CapitalLambda]Flat[zd_,zs_,\[CapitalOmega]M_,\[CapitalOmega]\[CapitalLambda]_]:= 1/(1+zs) NIntegrate[H0/Hubble\[CapitalLambda][z,\[CapitalOmega]M,1-\[CapitalOmega]M],{z,zd,zs}]  ;
#
# Dist\[CapitalLambda]Close[zd_,zs_,\[CapitalOmega]M_,\[CapitalOmega]\[CapitalLambda]_]:= (
# \[CapitalOmega]Kv=(1-\[CapitalOmega]M-\[CapitalOmega]\[CapitalLambda]);
# 1/(1+zs) 1/(-\[CapitalOmega]Kv)^(1/2) Sin[ NIntegrate[((-\[CapitalOmega]Kv)^(1/2) H0)/Hubble\[CapitalLambda][z,\[CapitalOmega]M,\[CapitalOmega]\[CapitalLambda]],{z,zd,zs}]  ]
# );
#
# Dist\[CapitalLambda]Open[zd_,zs_,\[CapitalOmega]M_,\[CapitalOmega]\[CapitalLambda]_]:= (
# \[CapitalOmega]Kv=(1-\[CapitalOmega]M-\[CapitalOmega]\[CapitalLambda]);
# 1/(1+zs) 1/\[CapitalOmega]Kv^(1/2) Sinh[ NIntegrate[(\[CapitalOmega]Kv^(1/2) H0)/Hubble\[CapitalLambda][z,\[CapitalOmega]M,\[CapitalOmega]\[CapitalLambda]],{z,zd,zs}]  ]
# );
#
#
# (* ::Text:: *)
# (*Linear growth function*)
#
#
# D1\[CapitalLambda]=Function[{z,\[CapitalOmega]M0,\[CapitalOmega]\[CapitalLambda]0},
# (*Peebles 1980*)
# (5\[CapitalOmega]M0)/2 Sqrt[\[CapitalOmega]\[CapitalLambda]0+(*\[CapitalOmega]R0 (1+z)^2*)+\[CapitalOmega]M0 (1+z)^3+(1-\[CapitalOmega]M0-\[CapitalOmega]\[CapitalLambda]0)(1+z)^2]NIntegrate[(1+x)/(\[CapitalOmega]\[CapitalLambda]0+(*\[CapitalOmega]R0 (1+z)^2*)+\[CapitalOmega]M0 (1+x)^3+(1-\[CapitalOmega]M0-\[CapitalOmega]\[CapitalLambda]0)(1+x)^2)^(3/2),{x,z,\[Infinity]}]
# ];
#
# (*
# D1\[CapitalLambda][z_,\[CapitalOmega]M0_,\[CapitalOmega]\[CapitalLambda]0_]:=Block[{x,a},
# a=1/(1+z);
# x=(\[CapitalOmega]\[CapitalLambda]0/\[CapitalOmega]M0)^(1./3.) a;
# 5./2. (\[CapitalOmega]M0/\[CapitalOmega]\[CapitalLambda]0)^(1/3) Sqrt[1+1/x^3]*NIntegrate[(y/(1+y^3))^(3/2),{y,0,x}]
# ]
# *)
#
# D1\[CapitalLambda]Approx=Compile[{{z,_Real},{\[CapitalOmega]M0,_Real},{\[CapitalOmega]\[CapitalLambda]0,_Real}},
# Block[{EzSq,\[CapitalOmega]Mz,\[CapitalOmega]\[CapitalLambda]z,D1},
# EzSq=\[CapitalOmega]\[CapitalLambda]0+(*\[CapitalOmega]R0 (1+z)^2*)+\[CapitalOmega]M0 (1+z)^3;
#
# \[CapitalOmega]Mz=(\[CapitalOmega]M0 (1+z)^3)/EzSq;
# \[CapitalOmega]\[CapitalLambda]z=\[CapitalOmega]\[CapitalLambda]0/EzSq;
# D1=(1+z)^-1 5./2. \[CapitalOmega]Mz (\[CapitalOmega]Mz^(4/7)-\[CapitalOmega]\[CapitalLambda]z+(1+\[CapitalOmega]Mz/2.0)(1+\[CapitalOmega]\[CapitalLambda]z/70.))^-1
# (*g=D1*(1+z)*)
# ],
# RuntimeAttributes->{Listable},Parallelization->True,CompilationTarget->"C"];
#
#
# (* ::Subsubsection:: *)
# (*Dark energy*)
#
#
# Hubble\[Omega][z_,\[CapitalOmega]M_,\[CapitalOmega]\[Omega]_,\[Omega]0_,\[Omega]a_]:=H0 Sqrt[\[CapitalOmega]M (1+z)^3+\[CapitalOmega]\[Omega] (1.+z)^(3(1+\[Omega]0+\[Omega]a)) Exp[(-3\[Omega]a z)/(1+z)]+(1-\[CapitalOmega]M-\[CapitalOmega]\[Omega])(1+z)^2];
# Dist\[Omega][zd_,zs_,\[CapitalOmega]M_,\[CapitalOmega]\[Omega]_,\[Omega]0_,\[Omega]a_]:= Block[{\[CapitalOmega]Kv,IntegralTmp1},
# \[CapitalOmega]Kv=(1.-\[CapitalOmega]M-\[CapitalOmega]\[Omega]);
# IntegralTmp1=NIntegrate[(\[CapitalOmega]M (1.+z)^3+\[CapitalOmega]\[Omega] (1.+z)^(3(1+\[Omega]0+\[Omega]a)) Exp[(-3\[Omega]a z)/(1+z)]+\[CapitalOmega]Kv (1.+z)^2)^-.5,{z,zd,zs}];
# (*
# IntegralTmp1=NIntegrate[1/Sqrt[\[CapitalOmega]M (1+z)^3+\[CapitalOmega]\[Omega] (1+z)^(3(1+\[Omega]))+\[CapitalOmega]Kv (1+z)^2],{z,zd,zs}];
# *)
# 1./(1.+zs) Which[
# \[CapitalOmega]Kv ==0,IntegralTmp1,
# \[CapitalOmega]Kv <0,1./Sqrt[-\[CapitalOmega]Kv] Sin[Sqrt[-\[CapitalOmega]Kv] IntegralTmp1],
# \[CapitalOmega]Kv >0,1./Sqrt[\[CapitalOmega]Kv] Sinh[ Sqrt[\[CapitalOmega]Kv] IntegralTmp1]
# ]
# ];
#
#
# Hubble\[Omega]Flat[z_,\[CapitalOmega]M_,\[Omega]_]:=H0 Sqrt[\[CapitalOmega]M (1+z)^3+(1-\[CapitalOmega]M)(1+z)^(3(1+\[Omega]))];
# Dist\[Omega]Flat[zd_,zs_,\[CapitalOmega]M_,\[Omega]_]:= 1/(1+zs) NIntegrate[H0/Hubble\[Omega]Flat[z,\[CapitalOmega]M,\[Omega]],{z,zd,zs}];
# (*
#
# Dist\[Omega]Flat[zd_,zs_,\[CapitalOmega]M_,\[Omega]_]:=2/(1+zs) (1/(Sqrt[1+zd] Sqrt[(1+zd)^(3 \[Omega]) (1-\[CapitalOmega]M)+\[CapitalOmega]M]) Hypergeometric2F1[1,1/2,1-1/(6 \[Omega]),((1+zd)^(3 \[Omega]) (1-\[CapitalOmega]M))/((1+zd)^(3 \[Omega]) (1-\[CapitalOmega]M)+\[CapitalOmega]M)]-1/(Sqrt[1+zs] Sqrt[(1+zs)^(3 \[Omega]) (1-\[CapitalOmega]M)+\[CapitalOmega]M]) Hypergeometric2F1[1,1/2,1-1/(6 \[Omega]),((1+zs)^(3 \[Omega]) (1-\[CapitalOmega]M))/((1+zs)^(3 \[Omega]) (1-\[CapitalOmega]M)+\[CapitalOmega]M)]);
#
#
# Dist\[Omega]Flat[zd_,zs_,\[CapitalOmega]M_,\[Omega]_]:= 1/(1+zs) NIntegrate[H0/Hubble\[Omega]Flat[z,\[CapitalOmega]M,\[Omega]],{z,zd,zs}];
#
# Dist\[Omega]Flat[zd_,zs_,\[CapitalOmega]M_,\[Omega]_]:=If[\[CapitalOmega]M>0 &&\[Omega]!=0,1/(1+zs) 2/\[CapitalOmega]M (Sqrt[((1+zd)^(3 \[Omega]) (1-\[CapitalOmega]M)+\[CapitalOmega]M)/(1+zd)] Hypergeometric2F1[1,1/2-1/(6 \[Omega]),1-1/(6 \[Omega]),((1+zd)^(3 \[Omega]) (-1+\[CapitalOmega]M))/\[CapitalOmega]M]-Sqrt[((1+zs)^(3 \[Omega]) (1-\[CapitalOmega]M)+\[CapitalOmega]M)/(1+zs)] Hypergeometric2F1[1,1/2-1/(6 \[Omega]),1-1/(6 \[Omega]),((1+zs)^(3 \[Omega]) (-1+\[CapitalOmega]M))/\[CapitalOmega]M]),2 /((1+zs) (1+3 \[Omega])) ((1+zd)^(-(1/2) (1+3 \[Omega]))-(1+zs)^(-(1/2) (1+3 \[Omega])))];
# *)
#
#
#
# (*
# Coeff*Hypergeometric2F1[a,b,c,z]/.{Coeff->Sqrt[((1+zd)^(3 \[Omega]) (1-\[CapitalOmega]M)+\[CapitalOmega]M)/(1+zd)],a->1,b->1/2-1/(6 \[Omega]),c->1-1/(6 \[Omega]),z->((1+zd)^(3 \[Omega]) (-1+\[CapitalOmega]M))/\[CapitalOmega]M}/.{zd->zdv,\[CapitalOmega]M->\[CapitalOmega]Mv,\[Omega]->\[Omega]v}
# Coeff*(1-z)^-a*Hypergeometric2F1[a,c-b,c,z/(z-1)]/.{Coeff->Sqrt[((1+zd)^(3 \[Omega]) (1-\[CapitalOmega]M)+\[CapitalOmega]M)/(1+zd)],a->1,b->1/2-1/(6 \[Omega]),c->1-1/(6 \[Omega]),z->((1+zd)^(3 \[Omega]) (-1+\[CapitalOmega]M))/\[CapitalOmega]M}/.{zd->zdv,\[CapitalOmega]M->\[CapitalOmega]Mv,\[Omega]->\[Omega]v}
# {(1-z)^-a,c-b,z/(z-1)}/.{Coeff->Sqrt[((1+zd)^(3 \[Omega]) (1-\[CapitalOmega]M)+\[CapitalOmega]M)/(1+zd)],a->1,b->1/2-1/(6 \[Omega]),c->1-1/(6 \[Omega]),z->((1+zd)^(3 \[Omega]) (-1+\[CapitalOmega]M))/\[CapitalOmega]M}//Simplify
# *)
# (*Simplify[1/(1+zs) Integrate[H0/Hubble\[Omega]Flat[z,0,\[Omega]],{z,zd,zs}],{zd>=0 ,zs>=0,zd<=zs,Re[1/(zd-zs)]<=0}]*)
#
#
# (* ::Text:: *)
# (*Performances*)
#
#
# (* ::Input:: *)
# (*(**)
# (*{zdTmp1,zsTmp1}={0.25,1.5};*)
# (*Dist\[Omega][zdTmp1,zsTmp1,0.3,0.7,-1,0]*)
# (*Dist\[CapitalLambda]Flat[zdTmp1,zsTmp1,0.3,0.7]*)
# (*AbsoluteTiming[Table[Dist\[Omega][zdTmp1,zsTmp1,0.3,0.7,-1,0],{i,2*10^2}];]*)
# (*AbsoluteTiming[Table[Dist\[CapitalLambda]Flat[zdTmp1,zsTmp1,0.3,0.7],{i,2*10^2}];]*)
# (**)*)
#
#
# (* ::Subsection:: *)
# (*Priors*)
#
#
# (* ::Input:: *)
# (*(**)
# (*NIntegrate[1/Par2 1/(Sqrt[2\[Pi]]ParLog\[Sigma]) Exp[-((Log[Par2]-c)^2/(2ParLog\[Sigma]^2))]/.{ParLog\[Sigma]->0.1,ParMin->0.1,ParMax->100,c->1.3},{Par2,-\[Infinity],+\[Infinity]}]//Chop*)
# (*NIntegrate[Log[ParMax/ParMin]^-1 1/Par1*Log[38./2.3]^-1 1/Par2/.{ParLog\[Sigma]->0.1,ParMin->0.1,ParMax->100,c->0.3},{Par1,0.1,100},{Par2,2.3,38.}]//Chop*)
# (**)*)
#
#
# (* ::Input:: *)
# (*(**)
# (*Table[{i,NIntegrate[1/Par2 1/(Sqrt[2\[Pi]]ParLog\[Sigma]) Exp[-((Log[Par2]-cMFac*Par1^-i)^2/(2ParLog\[Sigma]^2))]*Log[ParMax/ParMin]^-1 1/Par1/.{ParLog\[Sigma]->0.1,ParMin->0.1,ParMax->100,cMFac->0.32},{Par1,0.1,100},{Par2,-\[Infinity],+\[Infinity]}]//Chop},{i,.1,.6,.1}]*)
# (**)*)
#
#
# (* ::Input:: *)
# (*(**)
# (*NIntegrate[1/Par2 1/(Sqrt[2\[Pi]]ParLog\[Sigma]) Exp[-((Log[Par2]-cMFac*Par1^1.11)^2/(2ParLog\[Sigma]^2))]*Log[ParMax/ParMin]^-1 1/Par1/.{ParLog\[Sigma]->0.1,ParMin->0.1,ParMax->100,cMFac->0.32},{Par1,0.1,100},{Par2,-\[Infinity],+\[Infinity]}]//Chop*)
# (*NIntegrate[1/Par2 1/(Sqrt[2\[Pi]]ParLog\[Sigma]) Exp[-((Log[Par2]-cMFac*Par1^1.12)^2/(2ParLog\[Sigma]^2))]*Log[ParMax/ParMin]^-1 1/Par1/.{ParLog\[Sigma]->0.1,ParMin->0.1,ParMax->100,cMFac->1.3},{Par1,0.1,100},{Par2,-\[Infinity],+\[Infinity]}]//Chop*)
# (**)*)
#
#
# (* ::Subsection:: *)
# (*SZ*)
#
#
# (* ::Text:: *)
# (*Frequence dependency with relativistic correction (Itoh, Kohyama, Nozawa 1998ApJ...502....7I  Eq.(3.1); Reese et al. 2002)*)
#
#
# fSZ[\[Nu]_,T_]:=Module[{x,\[Theta]e,\[Delta]SZ},
# x=1.7592530899762326`*^-11 \[Nu] (*hPlanckConstant*\[Nu]/kB/TCMB/.SubMKS/.SubMKS*);
# \[Theta]e=T*0.0019569497678997`(*T* keV/me/c^2/.SubMKS/.SubMKS*);
# \[Delta]SZ=-17.\[Theta]e/10.+123.*\[Theta]e^2/40.-1989.*\[Theta]e^3/280.+14403.*\[Theta]e^4/640.;
# Return[{(x*(E^x+1.)/(E^x-1.)-4.)(1+\[Delta]SZ),\[Delta]SZ,-2.(1+\[Delta]SZ)}]
# ];
#
# gSZ[\[Nu]_,T_]:=Module[{x,\[Theta]e,F,G,\[Delta]Rel},
# x=1.7592530899762326`*^-11 \[Nu] (*hPlanckConstant*\[Nu]/kB/TCMB/.SubMKS/.SubMKS*);
# \[Theta]e=T*0.0019569497678997`(*T* keV/me/c^2/.SubMKS/.SubMKS*);
# F=x Coth[x/2];
# G=x/Sinh[x/2];
# \[Delta]Rel=(x E^x)/(E^x-1) \[Theta]e(-10+47/2 F-42/5 F^2+7/10 F^3+7/5 G^2 (-3+F));
# x E^x/(E^x-1) (F-4)+\[Delta]Rel
# ];
#
# (*Solve[-4+Xt+\[Delta]1==(-4+Xt)(1+\[Delta]2),\[Delta]2]//Simplify*)
# \[Delta]SZIT98[\[Nu]_,T_]:=Block[{x,Xt,St,Y0,\[Theta]e,Y1,Y2,Y3,Y4,\[Delta]SZ1},
# x=1.7592530899762326`*^-11 \[Nu] (*hPlanckConstant*\[Nu]/kB/TCMB/.SubMKS/.SubMKS*);
# \[Theta]e=T*0.0019569497678997`(*T* keV/me/c^2/.SubMKS/.SubMKS*);
# Xt=x Coth[x/2];
# St=x/Sinh[x/2];
# Y0=-4+Xt;
# Y1=-10+47*Xt/2-42*Xt^2/5+7*Xt^3/10+St^2(-21/5+7*Xt/5);
# Y2=-15/2+1023*Xt/8-868*Xt^2/5+329*Xt^3/5-44*Xt^4/5+11*Xt^5/30+St^2(-434/5+658*Xt/5-242*Xt^2/5+143*Xt^3/30)+St^4(-44/5+187*Xt/60);
# Y3=15/2+2505*Xt/8-7098*Xt^2/5+14253*Xt^3/10-18594*Xt^4/35+12059*Xt^5/140-128*Xt^6/21+16*Xt^7/105+St^2(-7098/10+14253*Xt/5-102267*Xt^2/35+156767*Xt^3/140-1216*Xt^4/7+64*Xt^5/7)+St^4(-18594/35+205003*Xt/280-1920*Xt^2/7+1024*Xt^3/35)+St^6(-544/21+992*Xt/105);
# Y4=-135/32+30375/128*Xt-62391/10*Xt^2+614727/40*Xt^3-124389/10*Xt^4+355703/80*Xt^5-16568/21*Xt^6+7516/105*Xt^7-22/7*Xt^8+11/210*Xt^9+St^2*(-62391/20+614727/20*Xt-1368279/20*Xt^2+4624139/80*Xt^3-157396/7*Xt^4 +30064/7*Xt^5-2717/7*Xt^6+2761/210*Xt^7)+St^4(-124389/10+6046951/160*Xt-248520/7*Xt^2+481024/35*Xt^3-15972/7*Xt^4+18689/140*Xt^5)+St^6(-70414/21+465992/105*Xt-11792/7*Xt^2+19778/105*Xt^3)+St^8(-682/7+7601/210*Xt);
# \[Delta]SZ1=\[Theta]e*Y1+\[Theta]e^2*Y2+\[Theta]e^3*Y3+\[Theta]e^4*Y4;
# Return[{Y0+\[Delta]SZ1,\[Delta]SZ1/Y0}]
# ];
#
# \[Delta]SZCH98[\[Nu]_,T_]:=Block[{x,Xt,St,Y0,\[Theta]e,Y1,Y2,Y3,Y4,\[Delta]SZ1},
# x=1.7592530899762326`*^-11 \[Nu] (*hPlanckConstant*\[Nu]/kB/TCMB/.SubMKS/.SubMKS*);
# \[Theta]e=T*0.0019569497678997`(*T* keV/me/c^2/.SubMKS/.SubMKS*);
# Xt=x*Coth[x/2];
# St=x/Sinh[x/2];
# Y0=-4+Xt;
# Y1=-10+47*Xt/2-42/5*Xt^2+7/10*Xt^3+7/5*St^2*(Xt-3);
# Y2=-15/2+1023/8*Xt-868/5*Xt^2+329/5*Xt^3-44/5*Xt^4+11/30*Xt^5+St^2/30*(-2604+3948*Xt-1452*Xt^2+143*Xt^3)+St^4/60(-528+187*Xt);
# \[Delta]SZ1=\[Theta]e*Y1+\[Theta]e^2*Y2;
# Return[{Y0+\[Delta]SZ1,\[Delta]SZ1/Y0}]
# ];
#
#
# (*
# (*Relation between \[CapitalDelta]n/n and \[CapitalDelta]T/T*)
# n0[x_]:=1/(E^x-1);
# Solve[dlnn/dlnx==(D[Log[n0[E^lnx]], lnx] /. lnx -> Log[x]),dlnx]
# {{dlnx\[Rule]-((dlnn \[ExponentialE]^-x (-1+\[ExponentialE]^x))/x)}}
# *)
# (*
# (x*Coth[x/2]-4)/(x(E^x+1)/(E^x-1)-4)//Simplify
# *)
#
# (*
# \[Nu]Tmp1=\[Nu]SZ(*\[Nu]SZ*);
# TTmp1=TSZ(*TSZ*);
# TTmp1=10.;
# Plot[{fSZ[10^Log\[Nu],TTmp1][[2]],\[Delta]SZIT98[10^Log\[Nu],TTmp1][[2]],\[Delta]SZCH98[10^Log\[Nu],TTmp1][[2]]},{Log\[Nu],10,12},PlotStyle\[Rule]{Blue,Green,Red}]
# *)
#
# (*/.x\[Rule]hPlanckConstant*\[Nu]SZ/kB/TCMB/.SubMKS*)
#
#
# End[];
#
#
# EndPackage[];

# Supersonic-Flow

采用有旋特征线法设计超声速静压探针

src包含FORTAN和VB源程序

数值求解采用FORTRAN编写,大部分参照了Zucrow书中的程序，UI采用VB编写

supersonic.dll为FORTRAN生成的动态函数库，供VB调用

使用：运行supersonic.exe

PS：在linux下可能无法运行，请在windows下运行

PPS：在linux下可直接修改编译fortran程序

     $ gfortran SuperSonic.f -o supersonic
Ref:Zucrow M J, Hoffman J D. Gas dynamics[J]. New York: Wiley, 1976, 1976.

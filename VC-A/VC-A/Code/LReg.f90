

    subroutine xfit(j,xA,yA,nA, a, b, q)
    use Base
    implicit none
    !     driver for routine fit
    INTEGER NPT,j
    REAL SPREAD
    PARAMETER(NPT=100,SPREAD=0.1)
    INTEGER i,idum,mwt,NA
    REAL a,b,chi2,gasdev,q,siga,sigb,sig(NPT),x(NPT),y(NPT),dum,c,d,e
    real XA(NA), YA(NA),sigAB(NA), YB(NA)
    character*30 F,G
    F = 'Ignoring'
    G = 'Including'
    idum=-117
    do 11 i=1,NA
        sigAB(i)=SPREAD
        YB(i) = log10(YA(i))
        write(*,*)XA(i),YA(i),YB(i)
11  continue
    do 12 mwt=0,1
    
        call fit(xA,yB,NA,sigAB,mwt,a,b,siga,sigb,chi2,q)
        c = 10**a
        e = (log10(2.718281828459045))
        d = b*e

        if (mwt.eq.0) then
            write(sw,100)j,trim(F),spread,a,c,siga,b,d,sigb,chi2,q
100         Format('|   ',i4,'       |',A10,'  |   ',F6.3,'    |    ',f6.3,'    ',f6.3,'   |   ',f6.3,'  |    ',f6.3,'     ',f6.3,'    |  ',f6.3,'  | 'f8.3,' |     ',f6.3,'    |  ')
        else
            write(sw,100)j,trim(G),spread,a,c,siga,b,d,sigb,chi2,q
        endif


        write(*,'(1x,t5,a,f12.6,t24,a,f9.6)') 'A = ',a,'Uncertainty: ', siga
30      format('    The  intercept a for the linear regression :: ',F6.3,' Uncertainity :: ',f6.3,'   The result : ',f6.3 )
        write(*,'(1x,t5,a,f9.6,t24,a,f9.6)') 'B = ',b,'Uncertainty: ', sigb
        write(*,'(1x,t5,a,4x,f13.6)') 'Chi-squared: ',chi2
        write(*,'(1x,t5,a,f13.6)') 'Goodness-of-fit: ',q
12  continue
    END

    SUBROUTINE fit(x,y,ndata,sig,mwt,a,b,siga,sigb,chi2,q)
    INTEGER mwt,ndata
    REAL a,b,chi2,q,siga,sigb,sig(ndata),x(ndata),y(ndata)

    INTEGER i
    REAL sigdat,ss,st2,sx,sxoss,sy,t,wt,gammq
    sx=0.
    sy=0.
    st2=0.
    b=0.
    if(mwt.ne.0) then
        ss=0.
        do 11 i=1,ndata
            wt=1./(sig(i)**2)
            ss=ss+wt
            sx=sx+x(i)*wt
            sy=sy+y(i)*wt
11      continue
    else
        do 12 i=1,ndata
            sx=sx+x(i)
            sy=sy+y(i)
12      continue
        ss=float(ndata)
    endif
    sxoss=sx/ss
    if(mwt.ne.0) then
        do 13 i=1,ndata
            t=(x(i)-sxoss)/sig(i)
            st2=st2+t*t
            b=b+t*y(i)/sig(i)
13      continue
    else
        do 14 i=1,ndata
            t=x(i)-sxoss
            st2=st2+t*t
            b=b+t*y(i)
14      continue
    endif
    b=b/st2
    a=(sy-sx*b)/ss
    siga=sqrt((1.+sx*sx/(ss*st2))/ss)
    sigb=sqrt(1./st2)
    chi2=0.
    q=1.
    if(mwt.eq.0) then
        do 15 i=1,ndata
            chi2=chi2+(y(i)-a-b*x(i))**2
15      continue
        sigdat=sqrt(chi2/(ndata-2))
        siga=siga*sigdat
        sigb=sigb*sigdat
    else
        do 16 i=1,ndata
            chi2=chi2+((y(i)-a-b*x(i))/sig(i))**2
16      continue
        if(ndata.gt.2) then

            q = gammq(0.5*(ndata-2),0.5*chi2)
        endif
    endif
    return
    END

    FUNCTION gammq(a,x)
    REAL a,gammq,x
    !CU    USES gcf,gser
    REAL gammcf,gamser,gln
    if(x.lt.0..or.a.le.0.)pause 'bad arguments in gammq'
    if(x.lt.a+1.)then
        call gser(gamser,a,x,gln)
        gammq=1.-gamser
    else
        call gcf(gammcf,a,x,gln)
        gammq=gammcf
    endif
    return
    END

    SUBROUTINE gser(gamser,a,x,gln)
    INTEGER ITMAX
    REAL a,gamser,gln,x,EPS
    PARAMETER (ITMAX=100,EPS=3.e-7)
    !    USES gammln
    INTEGER n
    REAL ap,del,sum,gammln
    gln=gammln(a)
    if(x.le.0.)then
        if(x.lt.0.)pause 'x < 0 in gser'
        gamser=0.
        return
    endif
    ap=a
    sum=1./a
    del=sum
    do 11 n=1,ITMAX
        ap=ap+1.
        del=del*x/ap
        sum=sum+del
        if(abs(del).lt.abs(sum)*EPS)goto 1
11  continue
    pause 'a too large, ITMAX too small in gser'
1   gamser=sum*exp(-x+a*log(x)-gln)
    return
    END

    FUNCTION ran1(idum)
    INTEGER idum,IA,IM,IQ,IR,NTAB,NDIV
    REAL ran1,AM,EPS,RNMX
    PARAMETER (IA=16807,IM=2147483647,AM=1./IM,IQ=127773,IR=2836,NTAB=32,NDIV=1+(IM-1)/NTAB,EPS=1.2e-7,RNMX=1.-EPS)
    INTEGER j,k,iv(NTAB),iy
    SAVE iv,iy
    DATA iv /NTAB*0/, iy /0/
    if (idum.le.0.or.iy.eq.0) then
        idum=max(-idum,1)
        do 11 j=NTAB+8,1,-1
            k=idum/IQ
            idum=IA*(idum-k*IQ)-IR*k
            if (idum.lt.0) idum=idum+IM
            if (j.le.NTAB) iv(j)=idum
11      continue
        iy=iv(1)
    endif
    k=idum/IQ
    idum=IA*(idum-k*IQ)-IR*k
    if (idum.lt.0) idum=idum+IM
    j=1+iy/NDIV
    iy=iv(j)
    iv(j)=idum
    ran1=min(AM*iy,RNMX)
    return
    END


    SUBROUTINE gcf(gammcf,a,x,gln)
    INTEGER ITMAX
    REAL a,gammcf,gln,x,EPS,FPMIN
    PARAMETER (ITMAX=100,EPS=3.e-7,FPMIN=1.e-30)
    !   USES gammln
    INTEGER i
    REAL an,b,c,d,del,h,gammln
    gln=gammln(a)
    b=x+1.-a
    c=1./FPMIN
    d=1./b
    h=d
    do 11 i=1,ITMAX
        an=-i*(i-a)
        b=b+2.
        d=an*d+b
        if(abs(d).lt.FPMIN)d=FPMIN
        c=b+an/c
        if(abs(c).lt.FPMIN)c=FPMIN
        d=1./d
        del=d*c
        h=h*del
        if(abs(del-1.).lt.EPS)goto 1
11  continue
    pause 'a too large, ITMAX too small in gcf'
1   gammcf=exp(-x+a*log(x)-gln)*h
    return
    END


    FUNCTION gasdev(idum)
    INTEGER idum
    REAL gasdev
    !    USES ran1
    INTEGER iset
    REAL fac,gset,rsq,v1,v2,ran1
    SAVE iset,gset
    DATA iset/0/
    if (idum.lt.0) iset=0
    if (iset.eq.0) then
1       v1=2.*ran1(idum)-1.
        v2=2.*ran1(idum)-1.
        rsq=v1**2+v2**2
        if(rsq.ge.1..or.rsq.eq.0.)goto 1
        fac=sqrt(-2.*log(rsq)/rsq)
        gset=v1*fac
        gasdev=v2*fac
        iset=1
    else
        gasdev=gset
        iset=0
    endif
    return
    END


    FUNCTION gammln(xx)
    REAL gammln,xx
    INTEGER j
    DOUBLE PRECISION ser,stp,tmp,x,y,cof(6)
    SAVE cof,stp
    DATA cof,stp/76.18009172947146d0,-86.50532032941677d0,24.01409824083091d0,-1.231739572450155d0,.1208650973866179d-2,-.5395239384953d-5,2.5066282746310005d0/
    x=xx
    y=x
    tmp=x+5.5d0
    tmp=(x+0.5d0)*log(tmp)-tmp
    ser=1.000000000190015d0
    do 11 j=1,6
        y=y+1.d0
        ser=ser+cof(j)/y
11  continue
    gammln=tmp+log(stp*ser/x)
    return
    END




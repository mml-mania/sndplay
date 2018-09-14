VGMFILE$="test.vgm"
'VGMFILE$="fz_boss.vgm"
'VGMFILE$="fz_shop.vgm"

'hairetsu wo subroutine ni watasu houhou ga wakattara naosu
dim YM2151oct[16]
dim YM2151noteno[16]
dim YM2151kf[16]
dim YM2151algo[16]
dim YM2151tl[16,4]
dim keyon[16]
dim vbuf[16]
dim noteno[16]

var Hch, Hcnt
var fpos

'0:HDMI 1:Jack
audioout 1

print "Hit enter key to stop."
rc=vgmplay(VGMFILE$)
end

'
def vgmplay(vgmfile)
 dim vgm[1024*1024]
 dim fpos=0
 dim vgmsmp=0

 load "RAW:"+vgmfile,vgm
 if ! RESULT then
  print "File load error:"+VGMFILE$
  return #FALSE
 endif

 'file check
 if !chkvgm(vgm) then
  print "Input file error:"+VGMFILE$
  return #FALSE
 endif

 'get EoF offset
 eof=rdint(vgm,&h04,4)
 'print eof

 'get YM2151 clock
 ym2151clk=rdint(vgm,&h30,4)
 'print ym2151clk

 'get VGM data offset
 vgmdofst=rdint(vgm,&h34,4)
 'print vgmdofst

 'read vgm start
 fpos=&h34+vgmdofst
 while fpos < (eof + &h03)
  b=button(0,-1)
  if b==16 then break
  'print vgm[fpos]
  cmd=vgm[fpos]:inc fpos
  'print cmd
  if &h70 <= cmd && cmd <=&h7f then
   'wait n+1 samples
   wait=cmd-&h70+1
   inc vgmsmp, wait
   vgmwait(wait)
  elseif cmd==&h61 then
   'Wait n samples
   aa=vgm[fpos]:inc fpos
   dd=vgm[fpos]:inc fpos
   wait=aa+dd*256
   inc vgmsmp, wait
   vgmwait(wait)
  elseif cmd==&h62 then
   'wait 735 samples
   inc vgmsmp, 735
   vgmwait(735)
  elseif cmd==&h63 then
   'wait 882 samples
   inc vgmsmp, 882
   vgmwait(882)
  elseif cmd==&h66 then
   'end of sound data
   break
  elseif cmd==&h54 then
   'YM2151 primary, write value dd to register aa
   aa=vgm[fpos]:inc fpos
   dd=vgm[fpos]:inc fpos
   'print hex$(fpos), hex$(cmd), hex$(aa), hex$(dd)
   subYM2151 0,cmd,aa,dd
  elseif cmd==&ha4 then
   'YM2151 secondary, write value dd to register aa
   aa=vgm[fpos]:inc fpos
   dd=vgm[fpos]:inc fpos
   subYM2151 1,cmd,aa,dd
  elseif cmd==&hb9 then
   'HuC6280, write value dd to register aa
   aa=vgm[fpos]:inc fpos
   dd=vgm[fpos]:inc fpos
   'print hex$(fpos), hex$(cmd), hex$(aa), hex$(dd)
   subHuC6280 cmd,aa,dd,fpos-3
  elseif cmd==&h4f or cmd==&h50 then
   'not use(psg)
   inc fpos
  elseif &h30<=cmd and cmd<=&h3f then
   'not use(dd)
   inc fpos
  elseif &h40<=cmd and cmd<=&hbf then
   'not use(dd dd)
   inc fpos,2
  elseif &hc0<=cmd and cmd<=&hdf then
   'not use(dd dd dd)
   inc fpos,3
  elseif &he0<=cmd and cmd<=&hff then
   'not use(dd dd dd dd)
   inc fpos,4
  else
   print "Undef fpos:"+str$(fpos)+" &H"+hex$(cmd)
   input a$
  endif
 wend
 'fin
 for i = 0 to 15
  vol i,0
 next

 'debug print wavestr
 for i = 0 to 7
  'print "wave";i;":";
  for j = 0 to 15
   val=sound(&h80+i*16+j)
   'print format$("%02x",sound(&h80+i*16+j));",";
  next
  'print ""
 next

 return #TRUE
end

def chkvgm(vgm)
 vgmid$=chr$(vgm[0])+chr$(vgm[1])+chr$(vgm[2])+chr$(vgm[3])
 'print vgmid$
 if vgmid$ != "Vgm " then return #FALSE
 return #TRUE
end

def rdint(vgm,s, b)
 for i=0 to b-1
  'print vgm[s+i]
  v=v+vgm[s+i] * pow(256,i)
  inc fpos
 next
 return v
end

def vgmwait samples
 usleep samples/44.100*1000
end

def subYM2151 chip,cmd,aa,dd
 'print "YM2151:",chip,cmd,aa,dd:input a$
 if aa == &h08 then
  'key-on/off
  kon=(dd and &h78)>>3
  ch=(dd and &h07)+chip*8

  if kon==&h0f then
   keyon[ch]=1
   vol ch,vbuf[ch]
  else
   keyon[ch]=0
   vol ch,0
  endif

  'for debug
  if ch==0 then
   if kon==&h0f then
    'print "KeyOn :",hex$(fpos-3), kon, ch, YM2151oct[ch], YM2151noteno[ch], noteno[ch], note(noteno[ch]), YM2151kf[ch], vbuf[ch]:'input a$
   else
    'print "KeyOff:", kon, ch:'input a$
   endif
  endif

 elseif &h28 <= aa and aa <= &h2f then
  'KeyCode(Octave(3bit)+Note(4bit)
  ch=(aa-&h28)+chip*8
  YM2151oct[ch]=((dd and &h70)>>4)+0 '+1?
  YM2151noteno[ch]=dd and &h0F
  noteno[ch]=12*YM2151oct[ch]+YM2151noteno[ch] - (YM2151noteno[ch]>>2)
  freq ch, noteno[ch]+(YM2151kf[ch]/63)
 elseif &h30 <= aa and aa <= &h37 then
  'Key Fraction(6bit)
  ch=(aa-&h30)+chip*8
  YM2151kf[ch]=(dd and &hfc)>>2
  freq ch, noteno[ch]+(YM2151kf[ch]/63)
 elseif &h20 <= aa and aa <= &h27 then
  'Algorithm(3bit)
  ch=(aa-&h20)+chip*8
  YM2151algo[ch]=(dd and &h07)
  'if ch==0 then print "YM2151algo[", ch, "]=", YM2151algo[ch]
  vbuf[ch]=calcvol(YM2151algo[ch], YM2151tl[ch,0], YM2151tl[ch,1], YM2151tl[ch,2], YM2151tl[ch,3])
  if keyon[ch]!=0 then
   vol ch,vbuf[ch]
  endif
 elseif &h60 <= aa and aa <= &h7f then
  'Total Level(7bit)
  ch=((aa-&h60) mod 8)+chip*8
  op=(aa-&h60) div 8
  if op==1 then op=2 elseif op==2 then op=1
  YM2151tl[ch,op]=(dd and &h7f)
  'if ch==0 then print "YM2151tl[", ch, "][", op, "]=", YM2151tl[ch,op]
  vbuf[ch]=calcvol(YM2151algo[ch], YM2151tl[ch,0], YM2151tl[ch,1], YM2151tl[ch,2], YM2151tl[ch,3])
  if keyon[ch]!=0 then
   vol ch,vbuf[ch]
  endif
 elseif aa==&h02 then
  'wave select(sndplay only)
  ch=(dd and &hf0 >> 4)+chip*8
  wavno=dd and &h0f
  print "ch:",ch,"wavno:",wavno
  sound 8*ch+5,wavno
 endif
end

def subHuC6280 cmd,aa,dd,fpos
 if aa == &h00 then
  'channel select
  Hch = dd and &h07
  'print "Hch:", Hch
 elseif aa == &h06 then
  'wave str
  if Hch == 0 then
   if Hcnt < 32 then
    'initialize, ignore
   else
    wavno=(Hcnt-32) div 32
    wavaddr=((Hcnt-32) div 2) mod 16
    if (Hcnt mod 2) == 0 then
     'upper->lower
     sound &h80+(wavno*16)+wavaddr,dd
    else
     'lower->upper
     wavval=sound(&h80+(wavno*16)+wavaddr)
     sound &h80+(wavno*16)+wavaddr,wavval+dd*16
    endif
    'print "wavno:",wavno
   endif
   'print "Hch:",Hch,"Hcnt:",Hcnt,"wavno:",wavno,"dd:",dd
   inc Hcnt
  endif
 endif

end

'tl -> vol
'korede iinoka?
def calcvol(algo, tl0, tl1, tl2, tl3)
 if 0 <= algo and algo <= 3 then
  vv=(127-tl3)/2
 elseif 4 == algo then
  vv=((127-tl1)+(127-tl3))/2
 elseif 5 == algo or algo == 6 then
  vv=((127-tl1)+(127-tl2)+(127-tl3))/2
 else
  vv=((127-tl0)+(127-tl1)+(127-tl2)+(127-tl3))/2
 endif
 return vv
end

def note(no)
 return 440.0*pow(2.0,(no-69)/12.0)
end

def freq ch,f
 R%=5592.4*f
 sound ch*8+3,(R%>>24) and 255
 sound ch*8+2,(R%>>16) and 255
 sound ch*8+1,(R%>> 8) and 255
 sound ch*8  , R%      and 255
end

def vol ch,v
 sound ch*8+4,v
end


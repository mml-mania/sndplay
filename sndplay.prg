VGMFILE$="test.vgm"
'VGMFILE$="fz_boss.vgm"
'VGMFILE$="fz_shop.vgm"

'hairetsu wo subroutine ni watasu houhou ga wakattara naosu
dim YM2151oct[8]
dim YM2151noteno[8]
dim YM2151kf[8]
dim YM2151algo[8]
dim YM2151tl[8,4]
dim noteno[8]

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
  b=button(1,-1)
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
   'YM2151, write value dd to register aa
   aa=vgm[fpos]:inc fpos
   dd=vgm[fpos]:inc fpos
   'print hex$(fpos), hex$(cmd), hex$(aa), hex$(dd)
   subYM2151 cmd,aa,dd
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
 for i = 0 to 7
  vol i,0
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

def subYM2151 cmd,aa,dd
 'print "YM2151:",cmd,aa,dd:input a$
 if aa == &h08 then
  'key-on/off
  kon=(dd and &h78)>>3
  ch=dd and &h07

  'tl -> vol
  if 0 <= YM2151algo[ch] and YM2151algo[ch] <= 3 then
   vv=(127-YM2151tl[ch,3])/2
  elseif 4 == YM2151algo[ch] then
   vv=((127-YM2151tl[ch,1])+(127-YM2151tl[ch,3]))/2
  elseif 5 <= YM2151algo[ch] and YM2151algo[ch] <= 6 then
   vv=((127-YM2151tl[ch,1])+(127-YM2151tl[ch,2])+(127-YM2151tl[ch,3]))/2
  else
   vv=((127-YM2151tl[ch,0])+(127-YM2151tl[ch,1])+(127-YM2151tl[ch,2])+(127-YM2151tl[ch,3]))/2
  endif
  'ima wa ch0 dake saisei suru
  'if kon then
  if ch==0 and kon==&h0f then
   freq ch, noteno[ch]+(YM2151kf[ch]/127)
   'vol ch, 255
   vol ch,vv
  else
   vol ch,0
  endif
  'for debug
  if ch==0 then
   if kon==&h0f then
    print "KeyOn :",kon, ch, YM2151oct[ch], YM2151noteno[ch], noteno[ch], note(noteno[ch]), YM2151kf[ch], vv:'input a$
   else
    'print "KeyOff:", kon, ch:'input a$
   endif
  endif

 elseif &h28 <= aa and aa <= &h2f then
  'KeyCode(Octave(3bit)+Note(4bit)
  ch=aa-&h28
  YM2151oct[ch]=(dd and &h70)>>4
  YM2151noteno[ch]=dd and &h0F
  noteno[ch]=12*YM2151oct[ch]+YM2151noteno[ch] - (YM2151noteno[ch]>>2)
 elseif &h30 <= aa and aa <= &h37 then
  'Key Fraction(6bit)
  ch=aa-&h30
  YM2151kf[ch]=(dd and &hfc)>>2
 elseif &h20 <= aa and aa <= &h27 then
  'Algorithm(3bit)
  ch=aa-&h20
  YM2151algo[ch]=(dd and &h07)
  if ch==0 then print "YM2151algo[", ch, "]=", YM2151algo[ch]
 elseif &h60 <= aa and aa <= &h7f then
  'Total Level(7bit)
  ch=(aa-&h60) mod 8
  op=(aa-&h60) div 8
  if op==1 then op=2 elseif op==2 then op=1
  YM2151tl[ch,op]=(dd and &h7f)
  if ch==0 then print "YM2151tl[", ch, "][", op, "]=", YM2151tl[ch,op]
 endif
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


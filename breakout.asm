            ORG    $8000 

main:       LD      SP,0 
            CALL    setupIM2
            LD      a,r 
            LD      (seed),a 
            LD      hl,ball 
            CALL    cpsprite 
main3:      CALL    reset 
main0:      LD      a,(ballinply) 
            AND     a 
            JR      z,main1 
            CALL    hittest 
            CALL    balllost 
            CALL    moveball 
            CALL    ballhitbrk 
main1:      CALL    movepad 
            HALT    
            LD      a,(ballinply) 
            AND     a 
            JR      z,main2 
            CALL    prntball 
main2:      CALL    prntpad 
            LD      a,(lives) 
            AND     a 
            JR      nz,main4 
            CALL    gameover 
            JR      main3 
main4:      XOR     a 
            LD      (firstframe),a 
            LD      a,(ballinply) 
            AND     a 
            JR      nz,main0 
            LD      hl,releasetmr 
            DEC     (hl) 
            JR      nz,main0 
            LD      a,1 
            LD      (ballinply),a 
            CALL    resetball 
            JR      main0 

reltime     EQU     (50*2) 

setrelease: XOR     a 
            LD      (ballinply),a 
            LD      a,reltime 
            LD      (releasetmr),a 
            RET     

maxlives    EQU     3 

reset:      CALL    clrscr 
            LD      bc,$1600 
            CALL    scraddr 
            EX      de,hl 
            LD      hl,title 
            CALL    prntstr 
            xor     a
            ld (scorel),a
            ld (scorem),a
            ld (scoreh),a
            CALL    prntscr 
            CALL    newgame 
            CALL    prntbrks 
            LD      a,maxlives 
            LD      (lives),a 
            CALL    prntlvs 
            LD      hl,$5800 
            LD      de,$5801 
            LD      bc,31 
            LD      a,4 
            LD      (hl),a 
            LDIR    
            LD      hl,$5820 
            LD      de,$5821 
            LD      bc,31 
            LD      a,5 
            LD      (hl),a 
            LDIR    
            LD      hl,$5ac0 
            LD      de,$5ac1 
            LD      bc,31 
            LD      a,2 
            LD      (hl),a 
            LDIR    
            LD      hl,$5ae0 
            LD      de,$5ae1 
            LD      bc,31 
            LD      a,3 
            LD      (hl),a 
            LDIR    
            RET     

pausetime   EQU     (4*50) 

gameover:   LD      bc,$0c0c 
            CALL    scraddr 
            EX      de,hl 
            LD      hl,gmover 
            CALL    prntstr 
            LD      b,pausetime 
gameover0:  HALT    
            DJNZ    gameover0 
            RET     

newgame:    LD      hl,brickbm 
            LD      de,brickbm+1 
            LD      bc,15 
            LD      a,$ff 
            LD      (hl),a 
            LDIR    
            LD      a,16*8 
            LD      (nmbrks),a 
            CALL    setrelease 
            RET     

balllost:   LD      a,(hpby+1) 
            CP      180 
            RET     c 
            CALL    prntball0 
            XOR     a 
            CALL    setrelease 
            LD      bc,$ff00 
            CALL    sfx2 
            LD      a,(lives) 
            DEC     a 
            LD      (lives),a 
            CALL    prntlvs 
            RET     

initbpos    EQU     $8000 

resetball:  LD      hl,initbpos 
            LD      (hpbx),hl 
            LD      (hpby),hl 
            LD      hl,$0100 
            LD      (dy),hl 
            CALL    random 
            LD      hl,velocities 
            LD      d,0 
            AND     $7 
            ADD     a,a 
            LD      e,a 
            ADD     hl,de 
            LD      e,(hl) 
            INC     hl 
            LD      d,(hl) 
            LD      (dx),de 
            LD      a,1 
            LD      (firstframe),a 
            RET     

clrscr:     LD      hl,$4000 
            LD      de,$4001 
            LD      bc,$17ff 
            XOR     a 
            LD      (hl),a 
            LDIR    
            LD      hl,$5800 
            LD      de,$5801 
            LD      bc,$2ff 
            LD      a,$47 
            LD      (hl),a 
            LDIR    
            XOR     a 
            OUT     ($fe),a 
            RET     

neg16:      LD      a,e 
            CPL     
            LD      e,a 
            LD      a,d 
            CPL     
            LD      d,a 
            INC     de 
            RET     

lowbound    EQU     4 

ubound:     DB      0 

adddelta:   LD      a,d 
            AND     $80 
            JR      z,notneg 
            LD      a,lowbound 
            CP      h 
            JR      c,doadd 
            LD      hl,lowbound 
            CALL    neg16 
            JR      doadd 
notneg:     LD      a,(ubound) 
            CP      h 
            JR      nc,doadd 
            LD      h,a 
            LD      l,0 
            CALL    neg16 
doadd:      ADD     hl,de 
            RET     

velocities: DW      $fe00 
            DW      $fe80 
            DW      $ff00 
            DW      $ff80 
            DW      $0080 
            DW      $0100 
            DW      $0180 
            DW      $0200 


hittest:    XOR     A 
            LD      a,(dy+1) 
            AND     $80 
            RET     nz 
            LD      a,(hpby+1) 
            CP      152 
            RET     c 
            CP      160 
            RET     nc 
            XOR     a 
            LD      a,(padx) 
            LD      b,a 
            LD      a,(hpbx+1) 
            ADD     a,4 
            CP      b 
            RET     c 
            XOR     a 
            LD      a,(padx) 
            ADD     a,24 
            LD      b,a 
            LD      a,(hpbx+1) 
            ADD     a,4 
            CP      b 
            RET     nc 
            RET     z 
            LD      a,(padx) 
            LD      b,a 
            LD      a,(hpbx+1) 
            ADD     a,4 
            SUB     b 
            CALL    getxvel 
            LD      (dx),de 
            LD      de,(dy) 
            CALL    neg16 
            LD      (dy),de 
            LD      bc,$20f0 
            CALL    sfx 
            RET     


getxvel:    CALL    div3 
            ADD     a,a 
            LD      e,a 
            LD      d,0 
            LD      hl,velocities 
            ADD     hl,de 
            LD      e,(hl) 
            INC     hl 
            LD      d,(hl) 
            RET     


div3:       LD      b,0 
            CP      12 
            JR      c,div30 
            SUB     12 
            SET     2,b 
div30:      CP      6 
            JR      c,div31 
            SUB     6 
            SET     1,b 
div31:      CP      3 
            JR      c,div32 
            SUB     3 
            SET     0,b 
div32:      LD      a,b 
            RET     


ballhitbrk: XOR     a 
            LD      a,(hpby+1) 
            ADD     a,4 
            SRL     a 
            SRL     a 
            SRL     a 
            CP      3 
            RET     c 
            CP      10 
            JR      z,ballhit0 
            RET     nc 
ballhit0:   SUB     3 
            LD      b,a 
            LD      a,(hpbx+1) 
            ADD     a,4 
            SRL     a 
            SRL     a 
            SRL     a 
            SRL     a 
            AND     $f 
            LD      c,a 
            PUSH    bc 
            CALL    isbrkset 
            POP     bc 
            RET     nc 
            CALL    rstbrk 
            LD      de,(dy) 
            CALL    neg16 
            LD      (dy),de 
            RET     

moveball:   LD      a,(hpbx+1) 
            LD      (obx),a 
            LD      a,(hpby+1) 
            LD      (oby),a 
            LD      hl,(hpbx) 
            LD      de,(dx) 
            LD      a,247 
            LD      (ubound),a 
            CALL    adddelta 
            LD      (hpbx),hl 
            LD      (dx),de 
            LD      hl,(hpby) 
            LD      de,(dy) 
            LD      a,183 
            LD      (ubound),a 
            CALL    adddelta 
            LD      (hpby),hl 
            LD      (dy),de 
            RET     

movepad:    LD      a,$f7 
            IN      a,($fe) 
            CPL     
            LD      c,a 
            AND     $1 
            JR      z,k0 
            LD      a,(padx) 
            AND     a 
            JR      z,k0 
            DEC     a 
            DEC     a 
            LD      (padx),a 
k0:         LD      a,c 
            AND     $2 
            JR      z,k1 
            LD      a,(padx) 
            CP      230 
            JR      nc,k1 
            INC     a 
            INC     a 
            LD      (padx),a 
k1:         RET     

prntpad:    LD      a,(padx) 
            AND     $7 
            SRL     a 
            SLA     a 
            SLA     a 
            LD      d,0 
            LD      e,a 
            LD      hl,paddle 
            ADD     hl,de 
            EX      de,hl 
            LD      a,(padx) 
            SRL     a 
            SRL     a 
            SRL     a 
            LD      c,a 
            LD      b,20 
            CALL    scraddr 
            LD      b,4 
prntpad0:   LD      a,(de) 
            LD      (hl),a 
            INC     h 
            LD      (hl),a 
            DEC     h 
            INC     l 
            INC     de 
            DJNZ    prntpad0 
            RET     


prntball:   LD      a,(firstframe) 
            AND     a 
            JR      nz,prntball0 
            LD      a,(obx) 
            LD      c,a 
            LD      a,(oby) 
            LD      b,a 
            LD      hl,ball 
            CALL    prntfinexy 
prntball0:  LD      a,(hpbx+1) 
            LD      c,a 
            LD      a,(hpby+1) 
            LD      b,a 
            LD      hl,ball 
            CALL    prntfinexy 
            RET     

prntbrk:    LD      de,brick 
            CALL    prntcell 
            INC     l 
            CALL    prntcell 
            RET     

prntrow:    LD      c,0 
prntrow2:   PUSH    bc 
            CALL    prntbrk 
            POP     bc 
prntrow0:   INC     l 
            INC     c 
            LD      a,16 
            CP      c 
            JR      nz,prntrow2 
            RET     

colours:    DB      $47 
            DB      $46 
            DB      $45 
            DB      $44 
            DB      $43 
            DB      $42 
            DB      $41 
            DB      $07 

prntbrks:   LD      hl,$5860 
            LD      de,$5861 
            LD      bc,(16*8)-1 
            XOR     a 
            LD      (hl),a 
            LDIR    
            LD      b,8 
            LD      c,3 
prntbrks1:  PUSH    bc 
            LD      b,c 
            LD      c,0 
            CALL    scraddr 
            CALL    prntrow 
            POP     bc 
            INC     c 
            DJNZ    prntbrks1 
            LD      hl,$5860 
            LD      ix,colours 
            LD      c,8 
prntbrks3:  LD      b,32 
            LD      a,(ix+0) 
prntbrks2:  LD      (hl),a 
            INC     hl 
            DJNZ    prntbrks2 
            DEC     c 
            INC     ix 
            JR      nz,prntbrks3 
            RET     

                    ; 
clrbrk:     XOR     a 
            LD      a,c 
            RL      a 
            LD      c,a 
            LD      a,b 
            ADD     a,3 
            LD      b,a 
            CALL    attaddr 
            LD      a,$47 
            LD      (hl),a 
            INC     l 
            LD      (hl),a 
            CALL    scraddr 
            CALL    prntbrk 
            RET     

getbrk:     XOR     a 
            LD      d,0 
            LD      a,b 
            ADD     a,a 
            LD      e,a 
            LD      hl,brickbm 
            ADD     hl,de 
            LD      a,c 
            AND     $8 
            JR      z,getbrk0 
            INC     hl 
getbrk0:    LD      a,c 
            AND     $7 
            JR      z,getbrk2 
            LD      b,a 
            XOR     a 
            LD      a,$80 
getbrk1:    SRL     a 
            DJNZ    getbrk1 
            RET     
getbrk2:    LD      a,$80 
            RET     

isbrkset:   CALL    getbrk 
            LD      b,(hl) 
            AND     b 
            JR      nz,isbrkset0 
            XOR     a 
            RET     
isbrkset0:  XOR     a 
            CCF     
            RET     

rstbrk:     PUSH    bc 
            CALL    getbrk 
            LD      b,(hl) 
            CPL     
            AND     b 
            LD      (hl),a 
            POP     bc 
            CALL    clrbrk 
            LD      b,16 
            CALL    noise 
            LD      b,5 
            CALL    addscore 
            CALL    prntscr 
            LD      hl,nmbrks 
            DEC     (hl) 
            RET     nz 
            LD      a,(obx) 
            LD      (hpbx+1),a 
            LD      a,(oby) 
            LD      (hpby+1),a 
            CALL    prntball0 
            CALL    newgame 
            CALL    prntbrks 
            RET     

addscore:   XOR     a 
            LD      hl,scorel 
            LD      a,(hl) 
            ADD     a,b 
            DAA     
            LD      (hl),a 
            INC     hl 
            LD      b,2 
addscore0:  LD      a,(hl) 
            ADC     a,0 
            DAA     
            LD      (hl),a 
            INC     hl 
            DJNZ    addscore0 
            RET     


getspr:     RL      a 
            RL      a 
            RL      a 
            RL      a 
            RL      a 
            LD      d,0 
            LD      e,a 
            ADD     hl,de 
            RET     

prntfinexy: LD      a,c 
            AND     $7 
            LD      hl,sprite 
            CALL    getspr 
            CALL    prntfiney 
            RET     

prntfiney:  LD      a,b 
            AND     $7 
            LD      d,0 
            LD      e,a 

            XOR     a 
            LD      a,b 
            SRL     a 
            SRL     a 
            SRL     a 
            LD      b,a 

            XOR     a 
            LD      a,c 
            SRL     a 
            SRL     a 
            SRL     a 
            LD      c,a 

            XOR     a 
            SBC     hl,de 
            EX      de,hl 

            CALL    prtspr 
            RET     


prtspr:     CALL    scraddr 
            PUSH    hl 
            CALL    prt2 
            POP     hl 
            INC     l 
            CALL    prt2 
            RET     

prt2:       CALL    prntcell 
            XOR     a 
            LD      a,32 
            ADD     a,l 
            LD      l,a 
            JR      nc,prt20 
            LD      a,8 
            ADD     a,h 
            LD      h,a 
prt20:      CALL    prntcell 
            RET     

prntcell:   LD      b,8 
            PUSH    hl 
printcell0: LD      a,(de) 
            LD      c,(hl) 
            XOR     c 
            LD      (hl),a 
            INC     h 
            INC     de 
            DJNZ    printcell0 
            POP     hl 
            RET     

getchr:     LD      h,0 
            LD      l,a 
            ADD     hl,hl 
            ADD     hl,hl 
            ADD     hl,hl 
            LD      de,$3d00 
            ADD     hl,de 
            RET     

prnthlfch:  LD      b,4 
            PUSH    hl 
prnthlfch0: LD      a,(de) 
            LD      (hl),a 
            INC     h 
            LD      (hl),a 
            INC     h 
            INC     de 
            DJNZ    prnthlfch0 
            POP     hl 
            RET     

prntchr:    PUSH    hl 
            CALL    getchr 
            LD      de,hl 
            POP     hl 
            LD      b,4 
            CALL    prnthlfch 
            PUSH    de 
            LD      de,32 
            ADD     hl,de 
            POP     de 
            JP      prnthlfch 


prntstr:    LD      a,(hl) 
            CP      $ff 
            RET     z 
            SUB     32 
            PUSH    hl 
            EX      de,hl 
            PUSH    hl 
            CALL    prntchr 
            POP     hl 
            INC     l 
            EX      de,hl 
            POP     hl 
            INC     hl 
            JR      prntstr 


prntlvs:    LD      a,(lives) 
            ADD     a,16 
            LD      hl,$401f 
            CALL    prntchr 
            RET     


prntdgts:   LD      c,a 
            AND     $f0 
            RR      a 
            RR      a 
            RR      a 
            RR      a 
            ADD     a,16 
            PUSH    hl 
            CALL    prntchr 
            POP     hl 
            LD      a,c 
            AND     $0f 
            ADD     a,16 
            INC     l 
            PUSH    hl 
            CALL    prntchr 
            POP     hl 
            RET     

prntscr:    LD      hl,scoreh 
            LD      de,$4000 
            LD      b,3 
prntscr0:   PUSH    bc 
            LD      a,(hl) 
            PUSH    hl 
            EX      de,hl 
            CALL    prntdgts 
            EX      de,hl 
            POP     hl 
            DEC     hl 
            INC     e 
            POP     bc 
            DJNZ    prntscr0 
            RET     

scraddr:    LD      a,b 
            SLA     a 
            SLA     a 
            SLA     a 
            SLA     a 
            SLA     a 
            OR      c 
            LD      l,a 
            XOR     a 
            LD      a,b 
            AND     $18 
            OR      $40 
            LD      h,a 
            RET     

attaddr:    LD      hl,0 
            XOR     a 
            LD      l,b 
            RL      l 
            RL      l 
            RL      l 

            RL      l 
            RL      h 

            RL      l 
            RL      h 


            LD      a,c 
            OR      l 
            LD      l,a 
            LD      a,$58 
            OR      h 
            LD      h,a 
            RET     

random:     LD      a,(seed) 
            LD      b,a 

            RRCA    
            RRCA    
            RRCA    
            XOR     0x1f 

            ADD     a,b 
            SBC     a,255 

            LD      (seed),a 
            RET     

scrl1spr:   LD      b,8 
            LD      de,16 
scrl1spr0:  XOR     a 
            SRL     (hl) 
            PUSH    hl 
            PUSH    af 
            ADD     hl,de 
            POP     af 
            RR      (hl) 
            POP     hl 
            INC     hl 
            DJNZ    scrl1spr0 
            RET     

scrlnspr:   PUSH    hl 
            PUSH    bc 
            CALL    scrl1spr 
            POP     bc 
            POP     hl 
            DJNZ    scrlnspr 
            RET     

scroll:     DB      0 

cpsprite:   PUSH    hl 
            CALL    clrspr 
            POP     hl 
            LD      b,8 
            LD      de,sprite 
            LD      a,0 
            LD      (scroll),a 
cpsprite0:  PUSH    hl 
            PUSH    bc 
            PUSH    de 
            LD      bc,8 
            LDIR    
            POP     hl 
            LD      a,(scroll) 
            AND     a 
            JR      z,noscroll 
            LD      b,a 
            CALL    scrlnspr 
noscroll:   LD      de,32 
            ADD     hl,de 
            EX      de,hl 
            POP     bc 
            POP     hl 
            LD      a,(scroll) 
            INC     a 
            LD      (scroll),a 
            DJNZ    cpsprite0 
            RET     

clrspr:     LD      hl,sprite 
            LD      de,sprite+1 
            LD      bc,(8*32)-1 
            XOR     a 
            LD      (hl),a 
            LDIR    
            RET     

noise:      PUSH    bc 
            LD      a,8 
            OUT     ($fe),a 
            LD      a,r 
            LD      b,a 
noise0:     DJNZ    noise0 
            XOR     a 
            OUT     ($fe),a 
            POP     bc 
            DJNZ    noise 
            RET     

sfx:        DI      
sfx1:       LD      a,8 
            OUT     ($fe),a 
            PUSH    bc 
            LD      b,c 
sfx0:       NOP     
            NOP     
            NOP     
            DJNZ    sfx0 
            XOR     a 
            OUT     ($fe),a 
            POP     bc 
            DEC     c 
            DJNZ    sfx1 
            EI      
            RET     
                    
sfx2:       DI      
sfx21:      LD      a,8 
            OUT     ($fe),a 
            PUSH    bc 
            LD      b,c 
sfx20:      NOP     
            NOP     
            NOP     
            DJNZ    sfx20 
            XOR     a 
            OUT     ($fe),a 
            POP     bc 
            INC     c 
            DJNZ    sfx21 
            EI      
            RET     

setupIM2:   di
            im 2
            ld a, $fd
            ld i, a
            ei
            ret

padx:       DB      $80 
hpbx:       DW      $8000 
hpby:       DW      $8000 
dx:         DW      $0100 
dy:         DW      $0100 
oby:        DB      0 
obx:        DB      0 
firstframe: DB      1 
seed:       DB      0 
nmbrks:     DB      16*8 
ballinply:  DB      0 
releasetmr: DB      50 
scorel:     DB      $00 
scorem      DB      $00 
scoreh:     DB      $00 
lives:      DB      3 

title:      DB      " BREAKOUT! NAKED HORSE BOY 2017",$ff 
gmover:     DB      "GAME OVER",$ff 
paddle:     DB      $3f 
            DB      $ff 
            DB      $fc 
            DB      0 

            DB      $0f 
            DB      $ff 
            DB      $ff 
            DB      0 

            DB      $03 
            DB      $ff 
            DB      $ff 
            DB      $c0 

            DB      0 
            DB      $ff 
            DB      $ff 
            DB      $f0 

brick:      DB      $00 
            DB      $7f 
            DB      $7f 
            DB      $7f 
            DB      $7f 
            DB      $7f 
            DB      $7f 
            DB      $00 

            DB      $00 
            DB      $fe 
            DB      $fe 
            DB      $fe 
            DB      $fe 
            DB      $fe 
            DB      $fe 
            DB      $00 


ball:       DB      0 
            DB      0 
            DB      $18 
            DB      $3c 
            DB      $3c 
            DB      $18 
            DB      0 
            DB      0 

brickbm:    DW      $ffff 
            DW      $ffff 
            DW      $ffff 
            DW      $ffff 
            DW      $ffff 
            DW      $ffff 
            DW      $ffff 
            DW      $ffff 

            DW      0 
            DW      0 
            DW      0 
            DW      0 

sprite:     DW      0 

            org    $fdfd

            loop 257
            db $fe
            lend

            org    $fefe
           
            ei
            ret

            ; export a snapshot

            output_szx "breakout.szx",$0000,main 









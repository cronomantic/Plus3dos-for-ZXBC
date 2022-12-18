#ifndef __PLUS3DOS__
#define __PLUS3DOS__

'-----------------------------------------------------------------------------------
'Copyright (c) 2022, Cronomantic (Sergio Chico)
''
'Redistribution and use in source and binary forms, with or without
'modification, are permitted provided that the following conditions
'are met:
'1. Redistributions of source code must retain the above copyright
''   notice, this list of conditions and the following disclaimer.
'2. Neither the name of copyright holders nor the names of its
''   contributors may be used to endorse or promote products derived
''   from this software without specific prior written permission.
''
'THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
'“AS IS” AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
'TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
'PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL COPYRIGHT HOLDERS OR CONTRIBUTORS
'BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
'CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
'SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
'INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
'CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
'ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
'POSSIBILITY OF SUCH DAMAGE.
'----------------------------------------------------------------------------------

#define createPlus3DosCall(label, address) \
label: \
    CALL PLUS3_DOS_SETUP_BANKS \
    CALL address \
    CALL PLUS3_DOS_RESTORE_BANKS \
    RET \



ASM


PLUS3_DOS_BANKM      EQU $5B5C
PLUS3_DOS_BANK678    EQU $5B67
;PLUS3_DOS_BANK_PORT  EQU $7FFD
;PLUS3_DOS_BANK2_PORT EQU $1FFD

JP PLUS3_DOS_END

PLUS3_DOS_SETUP_BANKS:
PROC
    PUSH AF
    DI
    EXX

    LD A, (PLUS3_DOS_BANK678)
    LD L, A
    AND %00000111
    LD H, A
    LD A, L
    AND %11111000
    OR %00000100                  ;Rom 2/3 selection (+3dos / 48 basic)
    LD BC,$1FFD                   ;BC=1FFD
    OUT (C), A                     ;update port
    LD (PLUS3_DOS_BANK678), A

    LD A, (PLUS3_DOS_BANKM)
    LD L, A
    AND %11101000                 ;Change only bank bits
    OR %00000111                  ;Set ROM +3dos & Bank 7
    LD B, $7F                     ;BC=7FFD
    OUT (C), A                    ;update port
    LD (PLUS3_DOS_BANKM), A

    EXX
    EI
    POP AF

    RET

ENDP

PLUS3_DOS_RESTORE_BANKS:
PROC
    PUSH AF
    DI
    EXX
    ;HL & BC have the values of the previous routine

    LD A, L                       ;BC=7FFD
    OUT (C), A                    ;update port
    LD (PLUS3_DOS_BANKM), A

    LD A, (PLUS3_DOS_BANK678)
    AND $11111000
    OR H
    LD B, $1F                     ;BC=1FFD
    OUT (C), A                    ;update port
    LD (PLUS3_DOS_BANK678), A

    EXX
    EI
    POP AF
    RET
ENDP


createPlus3DosCall(PLUS3_DOS_INIT,$0100)
createPlus3DosCall(PLUS3_DOS_VERSION,$0103)
createPlus3DosCall(PLUS3_DOS_OPEN,$0106)
createPlus3DosCall(PLUS3_DOS_CLOSE,$0109)
createPlus3DosCall(PLUS3_DOS_ABANDON,$010C)
createPlus3DosCall(PLUS3_DOS_REF_HEAD,$010F)
createPlus3DosCall(PLUS3_DOS_READ,$0112)
createPlus3DosCall(PLUS3_DOS_WRITE,$0115)
createPlus3DosCall(PLUS3_DOS_SET_1346,$013F)
createPlus3DosCall(PLUS3_DOS_GET_1346,$013C)
createPlus3DosCall(PLUS3_DOS_OFF_MOTOR,$019c)
createPlus3DosCall(PLUS3_DOS_ON_MOTOR,$0196)
createPlus3DosCall(PLUS3_DOS_BYTE_READ,$0118)
createPlus3DosCall(PLUS3_DOS_BYTE_WRITE,$011B)
createPlus3DosCall(PLUS3_DOS_FREE_SPACE,$0121)
createPlus3DosCall(PLUS3_DOS_RENAME,$0127)
createPlus3DosCall(PLUS3_DOS_DELETE,$0124)
createPlus3DosCall(PLUS3_DOS_SET_DRIVE,$012D)
createPlus3DosCall(PLUS3_DOS_SET_USER,$0130)
createPlus3DosCall(PLUS3_DOS_GET_POS,$0133)
createPlus3DosCall(PLUS3_DOS_SET_POS,$0136)
createPlus3DosCall(PLUS3_DOS_GET_EOF,$0139)
createPlus3DosCall(PLUS3_DOS_FLUSH,$0142)
createPlus3DosCall(PLUS3_DOS_SET_ACCESS,$0145)
createPlus3DosCall(PLUS3_DOS_SET_ATTR,$0148)
createPlus3DosCall(PLUS3_DOS_SET_MESS,$014E)

PLUS3_DOS_END:
;==========================================================================
;                          IMPORTANT WARNING!
;==========================================================================
;  The label PLUS3_DOS_END must be allocated always below $C000!
;  When doing calls to +3DOS, Page 7 must be allocated on the $C000-$FFFF
;  range, so you could lose access to your code there.
;  At compile time, generate a Map file to check the address of the symbol
;  just in case...
;==========================================================================

END ASM


SUB FASTCALL Plus3DosResetSys()
ASM
PROC
    XOR A
    LD BC, $7ffd
    DI
    OUT (C),A          ;update port
    LD B,$1F           ;BC=1FFD
    OUT (C),A          ;update port
    LD HL, 0
    EX (SP), HL
ENDP
END ASM
END Sub

'--------------------------------------------------------------------------------
'    +3DOS Error codes
'    
'    Almost all of the routines on the library return a uByte value.
'  In case of error, the value returned is the +3DOS error code returned.
'  These are all the possible values returned by +3DOS and their meaning:
'
'    Recoverable disk errors:
'    
'    0	Drive not ready
'    1	Disk is write protected
'    2	Seek fail
'    3	CRC data error
'    4	No data
'    5	Missing address mark
'    6	Unrecognised disk format
'    7	Unknown disk error
'    8	Disk changed whilst +3DOS was using it
'    9	Unsuitable media for drive
'    
'    Non-recoverable errors:
'    
'    20	Bad filename
'    21	Bad parameter
'    22	Drive not found
'    23	File not found
'    24	File already exists
'    25	End of file
'    26	Disk full
'    27	Directory full
'    28	Read-only file
'    29	File number not open (or open with wrong access)
'    30	Access denied (file is in use already)
'    31	Cannot rename between drives
'    32	Extent missing (which should be there)
'    33	Uncached (software error)
'    34	File too big (trying to read or write past 8 megabytes)
'    35	Disk not bootable (boot sector is not acceptable to DOS BOOT)
'    36	Drive in use (trying to re-map or remove a drive with files
'    	  open)
'
'    Otherwise, the value returned usually 255 on success, and sometimes other 
'  values depending of the routine (see the documentation for each case),
'  but it will be equal or higher than 128 always in case of success.
'
'--------------------------------------------------------------------------------

'================================================================================
'Plus3DosInitialize
'  Parameters:
'
'  Returns:
'   255 On success.
'   +3Dos Error code otherwise..
'
'  Do the following:
'    - Initialise +3DOS.
'    - Initialise disk drivers.
'    - Initialise cache and the RAMdisk.
'    - All files closed.
'    - All drives logged out.
'    - Default drive A: (if disk interface present), else M:.
'    - Default user 0.
'    - Retry count 15.
'    - Error messages disabled.
'--------------------------------------------------------------------------------
FUNCTION Plus3DosInitialize() AS uByte

  DIM r AS uByte = $FF

ASM
PROC
    LOCAL toEnd

    PUSH IX
    CALL PLUS3_DOS_INIT
    POP IX
    JR C, toEnd
    LD (IX-1), A  ;Return value
toEnd:
ENDP
END ASM

  RETURN r

END FUNCTION

'================================================================================
'Plus3DosSetBuffers    
'  Parameters:
'    ByVal firstBufferCache AS uByte : First buffer for cache
'    ByVal numBufferCache AS uByte : Number of cache sector buffers
'    ByVal firstBufferRamDisk AS uByte : First buffer for RAMdisk
'    ByVal numBufferRamDisk AS uByte : Number of RAMdisk sector buffers
'  (Note that numBufferCache + numBufferRamDisk <= 128)  
'
'  Returns:
'   255 On success.
'   +3Dos Error code otherwise..
'
' Rebuild the sector cache and RAMdisk.
' 
' This routine is used to make some store available to the user, or to
' return store to DOS.
' 
' Note that if the RAMdisk is moved, ot its size is changed, then all
' files thereon are erased.
' 
' Pages 1, 3, 4, 6 are considered as an array of 128 sector buffers
' (numbered 0...127), each of 512 bytes. The cache and RAMdisk occupy
' two separate (contiguous) areas of this array.
' 
' The location and size of the cache and RAMdisk can be specified
' separately; any remaining buffers are unused by DOS and are available
' to the caller.
' 
' Note that the sizes actually usedd may be smaller than those specified
' as in practice, there is a maxximum cache size and a minimum size of
' RAMdisk (4 sectors).
' 
' A cache size of 0 will still work but will seriously impair the floppy
' disk performance.
' 
' This routine will fail if there are any files open on drive M:.
'--------------------------------------------------------------------------------

FUNCTION Plus3DosSetBuffers(ByVal firstBufferCache AS uByte, _
                            ByVal numBufferCache AS uByte, _
                            ByVal firstBufferRamDisk AS uByte, _
                            ByVal numBufferRamDisk AS uByte) AS uByte

  DIM r AS uByte = $FF

ASM
PROC
    LOCAL toEnd

    LD D, (IX+5)  ;firstBufferCache
    LD E, (IX+7)  ; numBufferCache
    LD H, (IX+9)  ; firstBufferRamDisk
    LD L, (IX+11)  ; numBufferRamDisk
    PUSH IX
    CALL PLUS3_DOS_SET_1346
    POP IX
    JR C, toEnd
    LD (IX-1), A  ;Return value
toEnd:
ENDP
END ASM

  RETURN r

END FUNCTION

'================================================================================
'Plus3DosGetFirstBufferCache   
'  Parameters:
'
'  Returns:
'     First buffer of cache
'
' Gets the start of the cache.
'
'Pages 1, 3, 4, 6 are considered as an array of 128 sector buffers
'(numbered 0...127), each of 512 bytes. The cache and RAMdisk occupy
'two separate (contiguous) areas of this array.

'Any unused sector buffers may be used by the caller.
'--------------------------------------------------------------------------------
FUNCTION Plus3DosGetFirstBufferCache() AS uByte

  DIM r AS uByte = $0

ASM
PROC
    PUSH IX
    CALL PLUS3_DOS_GET_1346
    POP IX
    LD (IX-1), D  ;Return value
ENDP
END ASM

  RETURN r

END FUNCTION

'================================================================================
'Plus3DosGetNumBufferCache
'  Parameters:
'
'  Returns:
'     Number of cache sector buffers
'
' Gets the number of buffers of the cache
'
'Pages 1, 3, 4, 6 are considered as an array of 128 sector buffers
'(numbered 0...127), each of 512 bytes. The cache and RAMdisk occupy
'two separate (contiguous) areas of this array.

'Any unused sector buffers may be used by the caller.
'--------------------------------------------------------------------------------
FUNCTION Plus3DosGetNumBufferCache() AS uByte

  DIM r AS uByte = $0

ASM
PROC
    PUSH IX
    CALL PLUS3_DOS_GET_1346
    POP IX
    LD (IX-1), E  ;Return value
ENDP
END ASM

  RETURN r

END FUNCTION

'================================================================================
'Plus3DosGetFirstBufferRamDisk  
'  Parameters:
'
'  Returns:
'     First buffer of the RamDisk
'
' Gets the start of the RamDisk.
'
'Pages 1, 3, 4, 6 are considered as an array of 128 sector buffers
'(numbered 0...127), each of 512 bytes. The cache and RAMdisk occupy
'two separate (contiguous) areas of this array.

'Any unused sector buffers may be used by the caller.
'--------------------------------------------------------------------------------
FUNCTION Plus3DosGetFirstBufferRamDisk() AS uByte

  DIM r AS uByte = $0

ASM
PROC
    PUSH IX
    CALL PLUS3_DOS_GET_1346
    POP IX
    LD (IX-1), H  ;Return value
ENDP
END ASM

  RETURN r

END FUNCTION


'================================================================================
'Plus3DosGetNumBufferRamDisk
'  Parameters:
'
'  Returns:
'     Number of Ramdisk sector buffers
'
' Gets the number of buffers of the RamDisk
'
'Pages 1, 3, 4, 6 are considered as an array of 128 sector buffers
'(numbered 0...127), each of 512 bytes. The cache and RAMdisk occupy
'two separate (contiguous) areas of this array.

'Any unused sector buffers may be used by the caller.
'--------------------------------------------------------------------------------
FUNCTION Plus3DosGetNumBufferRamDisk() AS uByte

  DIM r AS uByte = $0

ASM
PROC
    PUSH IX
    CALL PLUS3_DOS_GET_1346
    POP IX
    LD (IX-1), L  ;Return value
ENDP
END ASM

  RETURN r

END FUNCTION

'================================================================================
'Plus3DosGetVersion
'  Parameters:
'
'  Returns:
'     String with the current OS Version
'--------------------------------------------------------------------------------
FUNCTION Plus3DosGetVersion() AS String

  DIM d AS uByte
  DIM e AS uByte

ASM
PROC
    PUSH IX
    CALL PLUS3_DOS_VERSION
    POP IX
    LD (IX-1), D  ;Return Issue
    LD (IX-2), E  ;Return Version
ENDP
END ASM

  RETURN STR(d) + "." + STR(e)

END FUNCTION

'================================================================================
'Plus3DOSOpen
'  Parameters:
'    ByVal fileName AS String : Filespec of file to open or create (no wildcards).
'    ByVal fileNumber AS uByte : File number [0..15].
'    ByVal accessMode AS uByte : Check below
'    ByVal openAction AS uByte : Check below
'    ByVal createAction AS uByte : Check below
'
'  Returns:
'   255 On success & if file newly created.
'   254 On success & if existing file opened.
'   +3Dos Error code otherwise.
'  
'  Create and/or open a file
'  
'  There is a choice of action depending on whether or not the file
'  already exists. The choices are 'open action' or 'create action'
'  If the file already exists, then the open action
'  is followed; otherwise the create action is followed.
'  
'  Open action
'  
'  0. Error - File already exists.
'  
'  1. Open the file, read the header (if any). Position file
'     pointer after header.
'  
'  2. Open the file, ignore any header. Position file pointer at
'     000000h (0).
'  
'  3. Assume given filename is 'filename.type'. Erase
'     'filename.BAK' (if it exists). Rename 'filename.type' to
'     'filename.BAK'. Follow create action.
'  
'  4. Erase existing version. Follow create action.
'  
'  Create action
'  
'  0. Error - File does not exist.
'  
'  1. Create and open new file with a header. Position file
'     pointer after header.
'  
'  2. Create and open new file without a header. Position file
'     pointer at 000000h (0).
'  
'  (Example: To simulate the tape action of... 'if the file exists open
'  it, otherwise create it with a header', set open action = 1, create
'  action = 1.)
'  
'  (Example: To open a file and report an error if it does not exist, set
'  open action = 1, create action = 0.)
'  
'  (Example: To create a new file with a header, first renaming any
'  existing version to '.BAK', set open action = 3, create action = 1.)
'  
'  Files with headers have their EOF position recorded as the smallest
'  byte position greater than all written byte positions.
'  
'  Files without headers have their EOF position recorded as the byte at
'  the start of the smallest 128 byte record position greater than all
'  written record positions.
'  
'  Soft-EOF is the character 1Ah (26) and is nothing to do with the EOF
'  position, only the routine DOS BYTE READ knows about soft-EOF.
'  
'  The header data area is 8 bytes long and may be used by the caller for
'  any purpose whatsoever. If open action = 1, and the file exists (and
'  has a header), then the header data is read from the file, otherwise
'  the header data is zeroised. The header data is available even if the
'  file does not have a header. Call DOS REF HEAD to access the header
'  data.
'  
'  Note that +3 BASIC makes use of the first 7 of these 8 bytes as
'  follows:
'  
'  +---------------+-------+-------+-------+-------+-------+-------+-------+
'  | BYTE		|   0	|   1	|   2	|   3	|   4	|   5	|   6	|
'  +---------------+-------+-------+-------+-------+-------+-------+-------+
'  | Program	    0	file length	8000h or LINE	offset to prog	|
'  | Numeric array	    1	file length	xxx	name	xxx	xxx	|
'  | Character array   2	file length	xxx	name	xxx	xxx	|
'  | CODE or SCREEN$   3	file length	load address	xxx	xxx	|
'  +-----------------------------------------------------------------------+
'  
'  (xxx = doesn't matter)
'  
'  If creating a file that will subsequently be LOADed within BASIC, then
'  these bytes should be filled with the relevant values.
'  
'  The access mode to the is selected with this bitmap:
'      Bits 0...2 values:
'          1 = exclusive-read
'          2 = exclusive-write
'          3 = exclusive-read-write
'          5 = shared-read
'      Bits 3...7 = 0 (reserved)
'
'  If the file is opened with exclusive-write or exclusive-read-write
'  access (and the file has a header), then the header is updated when
'  the file is closed.
'  
'  A file that is already open for shared-read access on another file
'  number may only be opened for shared-read access on this file number.
'  
'  A file that is already open for exclusive-read or exclusive-write or
'  exclusive-read-write access on another file number may not be opened
'  on this file number.
'
'  There are convenience constants to configure these parameters.
'--------------------------------------------------------------------------------

CONST Plus3DosAccessModeExclusiveRead AS uByte = 1
CONST Plus3DosAccessModeExclusiveWrite AS uByte  = 2
CONST Plus3DosAccessModeExclusiveReadWrite AS uByte = 3
CONST Plus3DosAccessModeSharedRead AS uByte = 5
CONST Plus3DosAccessModeSharedWrite AS uByte = 6
CONST Plus3DosAccessModeSharedReadWrite AS uByte = 7

CONST Plus3DosOpenActionError AS uByte = 0
CONST Plus3DosOpenActionReadHeader AS uByte = 1
CONST Plus3DosOpenActionIgnoreHeader AS uByte = 2
CONST Plus3DosOpenActionBackExistingAndCreate AS uByte = 3
CONST Plus3DosOpenActionEraseExistingAndCreate AS uByte = 4

CONST Plus3DosCreateActionError AS uByte = 0
CONST Plus3DosCreateActionWithHeader AS uByte = 1
CONST Plus3DosCreateActionWithoutHeader AS uByte = 2


FUNCTION Plus3DOSOpen(ByVal fileName AS String, _
                      ByVal fileNumber AS uByte, _
                      ByVal accessMode AS uByte, _
                      ByVal openAction AS uByte, _
                      ByVal createAction AS uByte) AS uByte

  DIM r AS uByte = $FF

  LET fileName = fileName + CHR$($FF)

ASM
BREAKPNT:
PROC
    LOCAL doEnd, doError

    LD A, (IX+9)  ; Access Mode
    AND %00000111
    LD C, A
    LD B, (IX+7)  ; FIlenumber
    LD E, (IX+11) ; Open action
    LD D, (IX+13) ; Create action
    LD L, (IX+4)  ; Filename
    LD H, (IX+5)
    INC HL
    INC HL        ; Skip size
    PUSH IX
    CALL PLUS3_DOS_OPEN ; Not carry -> Error
    POP IX
    JR NC, doError
    JR NZ, doEnd
    LD A, $FE     ;New file
doError:
    LD (IX-1), A  ;Return value
doEnd:
ENDP
END ASM

  RETURN r

END FUNCTION

'================================================================================
'Plus3DOSClose
'  Parameters:
'    ByVal fileNumber AS uByte : File number [0..15].
'
'  Returns:
'   255 On success.
'   +3Dos Error code otherwise.
'
'  Close a file.
'  
'  Write the header (if there is one).
'  
'  Write any outstanding data.
'  
'  Update the directory.
'  
'  Release the file number.
'  
'  All opened files must eventually be closed (or abandoned). A file
'  number cannot be reused until it is closed (or abandoned).  
'--------------------------------------------------------------------------------
FUNCTION Plus3DOSClose(ByVal fileNumber AS uByte) AS uByte

  DIM r AS uByte = $FF

ASM
PROC
    LOCAL doEnd

    LD B, (IX+5)  ; fileNumber
    PUSH IX
    CALL PLUS3_DOS_CLOSE ; Not carry -> Error
    POP IX
    JR C, doEnd
    LD (IX-1), A  ;Return value
doEnd:
ENDP
END ASM

  RETURN r

END FUNCTION

'================================================================================
'Plus3DOSAbandon
'  Parameters:
'    ByVal fileNumber AS uByte : File number [0..15].
'
'  Returns:
'   255 On success.
'   +3Dos Error code otherwise.
'
'  Abandon a file.
'  
'  Similar to DOS CLOSE, except that any header, or data, or directory
'  data yet to be written to disk is discarded. This routine should only
'  be used to force a file closed in the event that DOS CLOSE is unable
'  to close the file (for example, if the media is damaged or permanently
'  changed or removed).
'--------------------------------------------------------------------------------
FUNCTION Plus3DOSAbandon(ByVal fileNumber AS uByte) AS uByte

  DIM r AS uByte = $FF

ASM
PROC
    LOCAL doEnd

    LD B, (IX+5)  ; fileNumber
    PUSH IX
    CALL PLUS3_DOS_ABANDON ; Not carry -> Error
    POP IX
    JR C, doEnd
    LD (IX-1), A  ;Return value
doEnd:
ENDP
END ASM

  RETURN r

END FUNCTION


'================================================================================
'Plus3DOSReferenceHeader
'  Parameters:
'    ByVal fileNumber AS uByte : File number [0..15].
'    ByRef hasHeader AS uByte : 1 if file has a header, 0 otherwise.
'    ByRef headerAddressInPage7 : Address of header data in page 7
'
'  Returns:
'   255 On success.
'   +3Dos Error code otherwise.
'
'  Point at the header data for this file.
'  
'  The header data area is 8 bytes long and may be used by the caller for
'  any purpose whatsoever. It is available even if the file does not have
'  a header; however, only files with a header and opened with write
'  access will have the header data recorded on disk.
'  
'  Note that +3 BASIC uses these 8 bytes (see the note under DOS OPEN
'  which gives the details). If creating a file that will subsequently be
'  LOADed within BASIC, then those bytes should be filled with the
'  relevant values.
'--------------------------------------------------------------------------------
FUNCTION Plus3DOSReferenceHeader(ByVal fileNumber AS uByte, _
                                 ByRef hasHeader AS uByte, _
                                 ByRef headerAddressInPage7 AS uInteger) AS uByte

  DIM r AS uByte = $FF

ASM
PROC
    LOCAL toEnd, toError, doZeroFlag

    LD B, (IX+5)
    PUSH IX
    CALL PLUS3_DOS_REF_HEAD
    EX (SP),IX
    POP DE
    JR NC, toError

    LD L, (IX+6)
    LD H, (IX+7)    ;hasHeader
    CCF
    JR Z, doZeroFlag        ;Sets carry flag as zero
    SCF
doZeroFlag:
    RLA             ; C to bit 0
    AND 1           ;mask other bits
    LD (HL), A

    LD L, (IX+8)
    LD H, (IX+9)    ;headerAddressInPage7
    LD (HL), E
    INC HL
    LD (HL), D
    JR toEnd
toError:
    LD (IX-1), A
toEnd:
ENDP
END ASM

  RETURN r

END FUNCTION


'================================================================================
'Plus3DOSRead
'  Parameters:
'    ByVal fileNumber AS uByte : File number
'    ByVal nBank AS uByte : Page for C000h (49152)...FFFFh (65535)
'    ByVal addr AS uInteger : Address for bytes to be read
'    ByVal size AS uInteger : Number of bytes to read (0 means 64K)
'    ByRef remainBytes AS uInteger : Number of bytes remaining unread in error
'
'  Returns:
'   255 On success.
'   +3Dos Error code otherwise.
'
' Read bytes from a file into memory.
' 
' Advance the file pointer.
' 
' The destination buffer is in the following memory configuration:
' 
'     C000h...FFFFh (49152...65535) - Page specified in C
'     8000h...BFFFh (32768...49151) - Page 2
'     4000h...7FFFh (16384...32767) - Page 5
'     0000h...3FFFh (0...16383)     - DOS ROM
' 
' The routine does not consider soft-EOF.
' 
' Reading EOF will produce an error.
'--------------------------------------------------------------------------------
FUNCTION Plus3DOSRead(ByVal fileNumber AS uByte, _
                      ByVal nBank AS uByte, _
                      ByVal addr AS uInteger, _
                      ByVal size AS uInteger, _
                      ByRef remainBytes AS uInteger) AS uByte

  DIM r AS uByte = $FF

ASM
PROC
    LOCAL toEnd
              
    LD B, (IX+5)    ;Filenumber
    LD C, (IX+7)    ;Bank
    LD L, (IX+8)    ;Addr
    LD H, (IX+9)
    LD E, (IX+10)   ;Size
    LD D, (IX+11)
    PUSH IX
    CALL PLUS3_DOS_READ
    POP IX
    JR C, toEnd
    LD L, (IX-12)   ;remainBytes
    LD H, (IX-13)
    LD (HL), E
    INC HL
    LD (HL), D
    LD (IX-1), A
toEnd:
ENDP
END ASM

  RETURN r

END FUNCTION

'================================================================================
'Plus3DOSWrite
'  Parameters:
'    ByVal fileNumber AS uByte : File number
'    ByVal nBank AS uByte : Page for C000h (49152)...FFFFh (65535)
'    ByVal addr AS uInteger : Address for bytes to write
'    ByVal size AS uInteger : Number of bytes to write (0 means 64K)
'    ByRef remainBytes AS uInteger : Number of bytes remaining unwritten in error
'
'  Returns:
'   255 On success.
'   +3Dos Error code otherwise.
'
'  Write bytes to a file from memory.
'
'  Advance the file pointer.
'
'  The source buffer is in the following memory configuration:
' 
'     C000h...FFFFh (49152...65535) - Page specified in C
'     8000h...BFFFh (32768...49151) - Page 2
'     4000h...7FFFh (16384...32767) - Page 5
'     0000h...3FFFh (0...16383)     - DOS ROM
'--------------------------------------------------------------------------------
FUNCTION Plus3DOSWrite(ByVal fileNumber AS uByte, _
                       ByVal nBank AS uByte, _
                       ByVal addr AS uInteger, _
                       ByVal size AS uInteger, _
                       ByRef remainBytes AS uInteger) AS uByte

  DIM r AS uByte = $FF

ASM
PROC
    LOCAL toEnd
              
    LD B, (IX+5)    ;Filenumber
    LD C, (IX+7)    ;Bank
    LD L, (IX+8)    ;Addr
    LD H, (IX+9)
    LD E, (IX+10)   ;Size
    LD D, (IX+11)
    PUSH IX
    CALL PLUS3_DOS_WRITE
    POP IX
    JR C, toEnd
    LD L, (IX-12)   ;remainBytes
    LD H, (IX-13)
    LD (HL), E
    INC HL
    LD (HL), D
    LD (IX-1), A
toEnd:
ENDP
END ASM

  RETURN r

END FUNCTION


'================================================================================
'Plus3DOSByteRead
'  Parameters:
'    ByVal fileNumber AS uByte : File number
'    ByRef byteRead AS uByte : Byte read
'
'  Returns:
'   255 On success.& byte read <> 1Ah (26) (soft-EOF)
'   254 On success.& byte read = 1Ah (26) (soft-EOF)
'   +3Dos Error code otherwise.
'
'  Read a byte from a file.
'  
'  Advance the file pointer.
'  
'  Tests for soft-EOF (1Ah (26)). As this condition is not latched, it is
'  possible to read past soft-EOF.
'  
'  EOF is latched.
'  
'  The caller must decide whether or not soft-EOF is of interest. This
'  would normally be the case only when reading an ASCII file.
'  
'  Reading EOF will produce an error.
'--------------------------------------------------------------------------------
FUNCTION Plus3DOSByteRead(ByVal fileNumber AS uByte, _
                          ByRef byteRead AS uByte) AS uByte

  DIM r AS uByte = $FF

ASM
PROC
    LOCAL toEnd, toError
              
    LD B, (IX+5)    ;Filenumber
    PUSH IX
    CALL PLUS3_DOS_BYTE_READ
    POP IX
    JR NC, toError
    LD L, (IX+6)      ;byteRead
    LD H, (IX+7)
    LD (HL), C        ;Store value
    JR NZ, toEnd      ;Detect soft EOL
    LD (IX-1), $FE    ;This is soft EOL
    JR toEnd
toError:
    LD (IX-1), A
toEnd:
ENDP
END ASM

  RETURN r

END FUNCTION

'================================================================================
'Plus3DOSByteWrite
'  Parameters:
'    ByVal fileNumber AS uByte : File number
'    ByVal byteWrite AS uByte : Byte to write
'
'  Returns:
'   255 On success
'   +3Dos Error code otherwise.
'
'  Write a byte to a file.
'
'  Advance the file pointer.
'--------------------------------------------------------------------------------
FUNCTION Plus3DOSByteWrite(ByVal fileNumber AS uByte, _
                           ByVal byteWrite AS uByte) AS uByte

  DIM r AS uByte = $FF

ASM
PROC
    LOCAL toEnd

    LD B, (IX+5)    ;Filenumber
    LD C, (IX+7)    ;byteWrite
    PUSH IX
    CALL PLUS3_DOS_BYTE_WRITE
    POP IX
    JR C, toEnd
    LD (IX-1), A
toEnd:
ENDP
END ASM

  RETURN r

END FUNCTION


'================================================================================
'Plus3DOSFreeSpace
'  Parameters:
'    ByVal drive AS String : Drive, ASCII String 'A'...'P', only first character read
'    ByRef freeSpace AS uInteger : Free space (in kilobytes)
'
'  Returns:
'   255 On success
'   +3Dos Error code otherwise.
'
'How much free space is there on this drive?
'--------------------------------------------------------------------------------
FUNCTION Plus3DOSFreeSpace(ByVal drive AS String, ByRef freeSpace AS uInteger) AS uByte

  DIM r AS uByte = $FF

  IF LEN(drive) = 0 THEN RETURN $15

ASM
PROC
    LOCAL toEnd, toError

    LD L, (IX+4)      ;drive
    LD H, (IX+5)
    INC HL
    INC HL
    LD A, (HL)
    PUSH IX
    CALL PLUS3_DOS_FREE_SPACE
    POP IX
    JR NC, toError
    EX DE, HL
    LD L, (IX+6)      ;freeSpace
    LD H, (IX+7)
    LD (HL), E
    INC HL
    LD (HL), D
    JR toEnd
toError:
    LD (IX-1), A
toEnd:
ENDP
END ASM

  RETURN r

END FUNCTION

'================================================================================
'Plus3DOSRename
'  Parameters:
'    ByVal orig AS String : old filename (no wildcards)
'    ByVal dest AS String : new filename (no wildcards)
'
'  Returns:
'   255 On success
'   +3Dos Error code otherwise.
'
'  Rename an existing file.
'  
'  File must not be open on any file number. A file with the new filename
'  must not exist. The new name must specify, or default to, the same
'  drive as the old name.
'--------------------------------------------------------------------------------
FUNCTION Plus3DOSRename(ByVal orig AS String, ByVal dest AS String) AS uByte

  DIM r AS uByte = $FF

  LET orig = orig + CHR$($FF)
  LET dest = dest + CHR$($FF)

ASM
PROC
    LOCAL toEnd

    LD L, (IX+4)      ;orig
    LD H, (IX+5)
    INC HL            ;Skip size
    INC HL
    LD E, (IX+6)      ;dest
    LD D, (IX+7)
    INC DE            ;Skip size
    INC DE
    PUSH IX
    CALL PLUS3_DOS_RENAME
    POP IX
    JR C, toEnd
    LD (IX-1), A
toEnd:
ENDP
END ASM

  RETURN r

END FUNCTION

'================================================================================
'Plus3DOSDelete
'  Parameters:
'    ByVal fileName AS String : filename, wildcards are permitted
'
'  Returns:
'   255 On success
'   +3Dos Error code otherwise.
'
'  Delete the file
'--------------------------------------------------
FUNCTION Plus3DOSDelete(ByVal fileName AS String) AS uByte

  DIM r AS uByte = $FF

  LET fileName = fileName + CHR$($FF)

ASM
PROC
    LOCAL toEnd

    LD L, (IX+4)      ;fileName
    LD H, (IX+5)
    INC HL            ;Skip size
    INC HL
    PUSH IX
    CALL PLUS3_DOS_DELETE
    POP IX
    JR C, toEnd
    LD (IX-1), A
toEnd:
ENDP
END ASM

  RETURN r

END FUNCTION

'================================================================================
'Plus3DOSSetDefaultDrive
'  Parameters:
'    ByVal drive AS String : Drive, ASCII String 'A'...'P', only first character read
'
'  Returns:
'   255 On success
'   +3Dos Error code otherwise.
'
' Set the default drive (i.e. the drive implied by all filenames that do
' not specify a drive).
' 
' The default drive is initially A:.
' 
' Does not access the drive, but merely checks that there is a driver
' for it (which does not imply that the drive exists).
' 
' This only affects routines that take filename parameters.
'--------------------------------------------------------------------------------
FUNCTION Plus3DOSSetDefaultDrive(ByVal drive AS String) AS uByte

  DIM r AS uByte = $FF

  IF LEN(drive) = 0 THEN RETURN $15

ASM
PROC
    LOCAL toEnd, skip, toError

    LD L, (IX+4)
    LD H, (IX+5)
    INC HL            ; Skip string size
    INC HL
    LD A, (HL)
    CP $FF
    JR NZ, skip
    LD A, $15
    JR toError
skip:
    PUSH IX
    CALL PLUS3_DOS_SET_DRIVE
    POP IX
    JR C, toEnd
toError:
    LD (IX-1), A
toEnd:
ENDP
END ASM

  RETURN r
  
END FUNCTION


'================================================================================
'Plus3DOSGetDefaultDrive
'  Parameters:
'    ByRef drive AS String : ASCII String 'A'...'P', the current drive.
'
'  Returns:
'   255 On success
'   +3Dos Error code otherwise.
'
'  Get the current default drive
'--------------------------------------------------------------------------------
FUNCTION Plus3DOSGetDefaultDrive(ByRef drive AS String) AS uByte

  DIM r AS uByte = $FF
  DIM d AS uByte

ASM
PROC
    LOCAL toEnd

    LD A, $FF
    PUSH IX
    CALL PLUS3_DOS_SET_DRIVE
    POP IX
    LD (IX-2), A
    JR C, toEnd
    LD (IX-1), A
toEnd:
ENDP
END ASM

  IF r = $FF THEN LET drive = CHR$(d)
  RETURN r

END FUNCTION

'================================================================================
'Plus3DOSSetDefaultUser
'  Parameters:
'    ByVal user AS uByte : New Default user. [0...15]
'
'  Returns:
'   255 On success
'   +3Dos Error code otherwise.
'
'  Get the current default User 
'--------------------------------------------------------------------------------
FUNCTION Plus3DOSSetDefaultUser(ByVal user AS uByte) AS uByte

  DIM r AS uByte = $FF

ASM
PROC
    LOCAL toEnd, skip, toError

    LD A, (IX+5)
    CP $FF
    JR NZ, skip
    LD A, $15
    JR toError
skip:
    PUSH IX
    CALL PLUS3_DOS_SET_USER
    POP IX
    JR C, toEnd
toError:
    LD (IX-1), A
toEnd:
ENDP
END ASM

  RETURN r
  
END FUNCTION

'================================================================================
'Plus3DOSGetDefaultUser
'  Parameters:
'    ByRef user AS uByte : Default user.
'
'  Returns:
'   255 On success
'   +3Dos Error code otherwise.
'
'  Get the current default User 
'--------------------------------------------------------------------------------
FUNCTION Plus3DOSGetDefaultUser(ByRef user AS uByte) AS uByte

  DIM r AS uByte = $FF

ASM
PROC
    LOCAL toEnd, toError

    LD A, $FF
    PUSH IX
    CALL PLUS3_DOS_SET_USER
    POP IX
    JR NC, toError
    LD L, (IX+4)
    LD H, (IX+5)
    LD (HL), A
    JR toEnd
toError:
    LD (IX-1), A
toEnd:
ENDP
END ASM

  RETURN r

END FUNCTION

'================================================================================
'Plus3DOSGetFilePos
'  Parameters:
'      ByVal fileNumber AS uByte : File number
'      ByRef pos AS uLong : File pointer 000000h...FFFFFFh (0...16777215)
'
'  Returns:
'   255 On success
'   +3Dos Error code otherwise.
'
'  Get the file pointer.
'--------------------------------------------------------------------------------
FUNCTION Plus3DOSGetFilePos(ByVal fileNumber AS uByte, _
                            ByRef pos AS uLong) AS uByte

  DIM r AS uByte = $FF

ASM
PROC
    LOCAL toEnd, toError

    LD B, (IX+5)
    PUSH IX
    CALL PLUS3_DOS_GET_POS
    POP IX
    JR NC, toError
    LD C, E
    EX DE, HL
    LD L, (IX+4)
    LD H, (IX+5)
    LD (HL), E
    INC HL
    LD (HL), D
    INC HL
    LD (HL), C
    INC HL
    LD (HL), 0
    JR toEnd
toError:
    LD (IX-1), A
toEnd:
ENDP
END ASM

  RETURN r
  
END FUNCTION

'================================================================================
'Plus3DOSSetFilePos
'  Parameters:
'      ByVal fileNumber AS uByte : File number
'      ByVal pos AS uLong : File pointer 000000h...FFFFFFh (0...16777215)
'
'  Returns:
'   255 On success
'   +3Dos Error code otherwise.
'
'  Set the file pointer.
'  
'  Does not access the disk.
'  
'  Does not check (or care) if pointer is >= 8 megabytes.
'--------------------------------------------------------------------------------
FUNCTION Plus3DOSSetFilePos(ByVal fileNumber AS uByte, _
                            ByVal pos AS uLong) AS uByte

  DIM r AS uByte = $FF

  IF pos > $FFFFFF THEN RETURN $15

ASM
PROC
    LOCAL toEnd

    LD B, (IX+5)
    LD L, (IX+6)
    LD H, (IX+7)
    LD E, (IX+8)
    PUSH IX
    CALL PLUS3_DOS_SET_POS
    POP IX
    JR C, toEnd
    LD (IX-1), A
toEnd:
ENDP
END ASM

  RETURN r
  
END FUNCTION

'================================================================================
'Plus3DOSGetEOF
'  Parameters:
'      ByVal fileNumber AS uByte : File number
'      ByVal pos AS uLong : File pointer 000000h...FFFFFFh (0...16777215)
'
'  Returns:
'   255 On success
'   +3Dos Error code otherwise.
'
'  Get the end of file (EOF) file position greater than all written byte
'  positions.
'  
'  Does not affect the file pointer.
'  
'  Does not consider soft-EOF.
'--------------------------------------------------------------------------------
FUNCTION Plus3DOSGetEOF(ByVal fileNumber AS uByte, _
                        ByRef pos AS uLong) AS uByte

  DIM r AS uByte = $FF

ASM
PROC
    LOCAL toEnd, toError

    LD B, (IX+5)
    PUSH IX
    CALL PLUS3_DOS_GET_EOF
    POP IX
    JR NC, toError
    LD C, E
    EX DE, HL
    LD L, (IX+4)
    LD H, (IX+5)
    LD (HL), E
    INC HL
    LD (HL), D
    INC HL
    LD (HL), C
    INC HL
    LD (HL), 0
    JR toEnd
toError:
    LD (IX-1), A
toEnd:
ENDP
END ASM

  RETURN r
  
END FUNCTION

'================================================================================
'Plus3DOSFlushDrive
'  Parameters:
'    ByVal drive AS String : Drive, ASCII String 'A'...'P', only first character read
'
'  Returns:
'   255 On success
'   +3Dos Error code otherwise.
'
'  Write any pending headers, data, directory entries for this drive.
'  
'  This routine ensures that the disk is up to date. It can be called at
'  any time, even when files are open.
'--------------------------------------------------------------------------------
FUNCTION Plus3DOSFlushDrive(ByVal drive AS String) AS uByte

  DIM r AS uByte = $FF

  IF LEN(drive) = 0 THEN RETURN $15

ASM
PROC
    LOCAL toEnd

    LD L, (IX+4)
    LD H, (IX+5)
    INC HL            ; Skip string size
    INC HL
    LD A, (HL)
    PUSH IX
    CALL PLUS3_DOS_FLUSH
    POP IX
    JR C, toEnd
    LD (IX-1), A
toEnd:
ENDP
END ASM

  RETURN r
  
END FUNCTION

'================================================================================
'Plus3DOSSetAccess
'  Parameters:
'      ByVal fileNumber AS uByte : File number
'      ByVal accessBits AS uByte : Access mode required
'                                Bits 0...2 values:
'                                  1 = exclusive-read
'                                  2 = exclusive-write
'                                  3 = exclusive-read-write
'                                  5 = shared-read
'                                (all other bit settings reserved)
'                                Bits 3...7 = 0 (reserved)
'
'  Returns:
'   255 On success
'   +3Dos Error code otherwise.
'
'  Try to change the access mode of an open file.
'  
'  This routine will fail if the file is already open, in an incompatible
'  access mode, or if write access is required for a read-only file or
'  disk.
'  
'--------------------------------------------------------------------------------
FUNCTION Plus3DOSSetAccess(ByVal fileNumber AS uByte, _
                           ByVal accessBits AS uByte) AS uByte

  DIM r AS uByte = $FF

ASM
PROC
    LOCAL toEnd

    LD B, (IX+5)    ;Filenumber
    LD A, (IX+7)    ;accessBits
    AND %00000111
    LD C, A
    PUSH IX
    CALL PLUS3_DOS_SET_ACCESS
    POP IX
    JR C, toEnd
    LD (IX-1), A
toEnd:
ENDP
END ASM

  RETURN r

END FUNCTION

'================================================================================
'Plus3DOSSetAttributes
'  Parameters:
'      ByVal fileName AS String : file name (wildcards permitted)
'      ByVal attSet AS uByte : Attributes to set
'                                bit 0 = t3' Archive
'                                bit 1 = t2' System
'                                bit 2 = t1' Read-only
'                                bit 3 = f4'
'                                bit 4 = f3'
'                                bit 5 = f2'
'                                bit 6 = f1'
'      ByVal attClr AS uByte : Attributes to clear
'                                bit 0 = t3' Archive
'                                bit 1 = t2' System
'                                bit 2 = t1' Read-only
'                                bit 3 = f4'
'                                bit 4 = f3'
'                                bit 5 = f2'
'                                bit 6 = f1'
'
'  Returns:
'   255 On success
'   +3Dos Error code otherwise.
'
'  Set a file's attributes.
'  
'  Only the file attributes f1'...f4', t1'...t3' can be set or
'  cleared. The interface attributes f5'...f8' are always 0.
'  
'  This routine first sets the attributes specified in D, then clears
'  those attributes specified in E, i.e. E has priority.
'--------------------------------------------------------------------------------
FUNCTION Plus3DOSSetAttributes(ByVal fileName AS String, _
                               ByVal attSet AS uByte, _
                               ByVal attClr AS uByte) AS uByte

  DIM r AS uByte = $FF

  LET fileName = fileName + CHR$($FF)

ASM
PROC
    LOCAL toEnd

    LD L, (IX+4)  ; Filename
    LD H, (IX+5)
    LD D, (IX+7)  ; attSet
    LD E, (IX+9)  ; attClr
    INC HL
    INC HL        ; Skip size
    PUSH IX
    CALL PLUS3_DOS_SET_ATTR
    POP IX
    JR C, toEnd
    LD (IX-1), A
toEnd:
ENDP
END ASM

  RETURN r

END FUNCTION


'================================================================================
'Plus3DOSSetMsg
'  Parameters:
'      ByVal enable AS uByte: enable = 0 to deactivate, enable <> 0 to activate
'      ByVal addrAlert AS uInteger : Address of ALERT routine (if enabled)
'
'  Returns:
'    Address of previous ALERT routine (0 if none)
'
'
'--------------------------------------------------------------------------------
FUNCTION Plus3DOSSetMsg(ByVal enable AS uByte, ByVal addrAlert AS uInteger) AS uInteger

  DIM r AS uInteger = 0

ASM
PROC
    LOCAL nextP

    LD A, (IX+5)
    OR A
    JR Z, nextP
    LD A, $FF
nextP:
    LD L, (IX+6)
    LD H, (IX+7)
    PUSH IX
    CALL PLUS3_DOS_SET_MESS
    POP IX
    LD (IX-1), L
    LD (IX-2), H
ENDP
END ASM

  RETURN r

END FUNCTION

'================================================================================
'Plus3DOSMotorOn
'  Parameters:
'
'  Returns:
'
' Starts the motor of the current drive
'--------------------------------------------------------------------------------
SUB FASTCALL Plus3DOSMotorOn()
ASM
    PUSH IX
    CALL PLUS3_DOS_ON_MOTOR
    POP IX
END ASM
END SUB

'================================================================================
'Plus3DOSMotorOff
'  Parameters:
'
'  Returns:
'
' Stops the motor of the current drive
'--------------------------------------------------------------------------------
SUB FASTCALL Plus3DOSMotorOff()
ASM
    PUSH IX
    CALL PLUS3_DOS_OFF_MOTOR
    POP IX
END ASM
END SUB

#undef createPlus3DosCall

#endif

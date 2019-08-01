;    Various functions for the Interaction with the FAT16 Filesystem
;
;    Copyright (C) 2010 Frank Zoll
;    Copyright (C) 2010 Sprite_tm
;    Copyright (C) 2010,2013 Leo C.
;
;    This file is part of avrcpm.
;
;    avrcpm is free software: you can redistribute it and/or modify it
;    under the terms of the GNU General Public License as published by
;    the Free Software Foundation, either version 3 of the License, or
;    (at your option) any later version.
;
;    avrcpm is distributed in the hope that it will be useful,
;    but WITHOUT ANY WARRANTY; without even the implied warranty of
;    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;    GNU General Public License for more details.
;
;    You should have received a copy of the GNU General Public License
;    along with avrcpm.  If not, see <http://www.gnu.org/licenses/>.
;
;    $Id: dsk_fat16.asm 153 2014-11-12 12:59:42Z rapid $
;

; ============================================================================
; Prelimitary !
; 같같같같같같�
; Size of a Sector is fixed to 512 Bytes by Base - MMC Driver implementation
; The Functions below therefore assume a fixed Size of 512 Bytes per Sector.
; ============================================================================

#if FAT16_SUPPORT


; ############################################################################ 
;                       	Defines for FAT16 Structures
; ############################################################################ 

/*These are the Offsets to the Variables within the Bootsector of a FAT16
  Partition.
*/
;#define FAT16_BSO_SECSIZE	0x0b	; Offset to Sectorsize Word
#define FAT16_BSO_CLUSTSZ   	0x0d    ; Offset to Clustersize Byte
#define FAT16_BSO_RESSECT   	0x0e	; Offset to Number of Reserved Sectors
#define FAT16_BSO_VOLPTR    	0x1c    ; Offset to First VolumeSector
#define FAT16_BSO_SECPERFAT 	0x16    ; Offset to Number of Sectors per Fat
#define FAT16_BSO_NUMFATCP  	0x10	; Offset to Ammount of FAT Copys
#define FAT16_BSO_NUMDIRENT 	0x11	; Offset to Max. Root Dir. entries

#define	FAT16_FIRST_IMAGENAME 'A'	/* First letter of filename to search */
#define FAT16_LAST_IMAGENAME  'Z'	/* Last letter of filename to */
#define FAT16_IMAGENAME_PREFIX "CPMDSK_"
#define FAT16_IMAGENAME_SUFFIX "IMG"

; ############################################################################ 
; 				Start of Data Segment
; ############################################################################ 

	.dseg

fat_vars:
fat_partfound:   .byte   1	; (partition: 0= no found 0xff=found )
fat_parttbl: 	 .byte	 4	; first fat16 partition entry 
				; only startsector is needed
fat_clustersize: .byte   1	; sectors per cluster
fat_ptr2fat:     .byte   4	; pointer to the first fat sector
fat_ptr2dat:     .byte   4	; pointer to the first data sector

/*These variables define a cache that holds the last Cluster and Sector
  thats been searched vor.
*/
fat_last_dsk:	  .byte	 1	; number of disk with entry in cache
fat_log_clust:	  .byte	 2	; last searched logical cluster
fat_clust_offset: .byte	 1	; offset within the cluster
fat_clust_ptr:	  .byte	 4 	; sector of last real cluster

	.equ	o_fat_partfound	  = 0
	.equ	o_fat_parttbl	  = 1
	.equ	o_fat_clustersize = 5
	.equ	o_fat_ptr2fat	  = 6
	.equ	o_fat_ptr2dat	  = 10
	.equ	o_fat_last_dsk	  = 14
	.equ	o_fat_log_clust	  = 15
	.equ	o_fat_clust_offset= 17
	.equ	o_fat_clust_ptr	  = 18

.if FAT16_FATBUF

fat_last_fatsect:	.byte	1
	.equ	o_fat_last_fatsect = 22

fat_buf:	.byte	512

.endif	/* FAT16_FATBUF */

; ############################################################################ 
; 				Start of Code Segment
; ############################################################################ 

	.cseg

fat_cfname:	.db	FAT16_IMAGENAME_PREFIX, 0
fat_cfext:	.db	FAT16_IMAGENAME_SUFFIX, 0


; ============================================================================
; Function: Initialize internal FAT-Partition Variables
; ============================================================================
; Parameters
; ----------------------------------------------------------------------------
; Registers  :	none
; Variables  :	[out]	fat_parttabl
; ----------------------------------------------------------------------------
; Description:
; This Routine initializes the internal Variables, that point to the
; first Found FAT16 Partition.
; ============================================================================
fat_init_partitiontable:

	ldiw	y,fat_vars
	std 	y+o_fat_partfound, _0
	std	y+o_fat_parttbl+0, _0
	std	y+o_fat_parttbl+1, _0
	std	y+o_fat_parttbl+2, _0
	std	y+o_fat_parttbl+3, _0
	ret


; ============================================================================
; Function: Resets the Cache
; ============================================================================
; Parameters
; ----------------------------------------------------------------------------
; Registers  :	none
; Variables  :	[out]	fat_log_clust
;		[out]	fat_last_dsk
; ----------------------------------------------------------------------------
; Description:
; This Routine resets the internal Cache- Variables. After reset, the
; next read or write Command will initialize a scan of the FAT of
; the FAT16-Partition for the given sector.
; ============================================================================
fat_reset_cache:
	ldiw	y,fat_vars
	std 	y+o_fat_log_clust  ,_255
	std	y+o_fat_log_clust+1,_255
	std	y+o_fat_last_dsk   ,_255
.if FAT16_FATBUF
	std	y+o_fat_last_fatsect,_255
.endif

	ret

; ============================================================================
; Function: Saves FAT16 Partitiondata for later Scanning
; ============================================================================
; Parameters
; ----------------------------------------------------------------------------
; Registers  :	[in]	z		Pointer to the Partitondata
; Variables  :	[out]	fat_partfound	Boolean for "Partition found"
;		[out]	fat_parttbl	Pointer to Partitiontable
; ----------------------------------------------------------------------------
; Description:
; This funktion sets the internal Variables to the Start and Size
; of a given FAT16 Paritition. This Information will be used for a
; later scanning of the Partition. See Function "fat_scan_partition"
; for more information. 
; ============================================================================

fat_add_partition:
	
.if FAT16_DEBUG > 0
	printstring "fat16 part found"
	printnewline
.endif

	ldiw	y,fat_vars

; set fat16 partition found flag

	std 	y+o_fat_partfound,_255

; save data from first fat16 partition

	ldd	temp,z+PART_START
	std	y+o_fat_parttbl,temp
	ldd	temp,z+PART_START+1
	std	y+o_fat_parttbl+1,temp
	ldd	temp,z+PART_START+2
	std	y+o_fat_parttbl+2,temp
	ldd	temp,z+PART_START+3
	std	y+o_fat_parttbl+3,temp

	ret

; ============================================================================
; Read and Scann a FAT16 Partition for Imagedatefiles 
; ============================================================================
; Registers	: none
; Variables	: none
; ----------------------------------------------------------------------------
; This Routine reads the Bootblock and scanns it for a Diskimage
; ============================================================================


fat_scan_partition:

.if FAT16_DEBUG > 0
	printstring "fat16 scanning"
	printnewline
.endif

; Check if a FAT16 Partition was realy found
	lds	yl,fat_partfound
	cpi 	yl,0	
	breq	fat_scan_error 


.if FAT16_DEBUG > 0
	printstring "free entries in ptable ?"
	printnewline
.endif

; Check for free entries in partition table
	lds	yl,ndisks
	cpi	yl,MAXDISKS
	brge	fat_scan_error

.if FAT16_DEBUG > 0
	printstring "read fat bootblock."
	printnewline
.endif

; Scan partition start
	ldiw	z,fat_parttbl	
	ldd	xl,z+0		
	ldd	xh,z+1
	ldd	yl,z+2
	ldd	yh,z+3
	ldiw	z,hostbuf

; Load first sector from Partition
	rcall	mmcReadSect
	tst	temp
	breq	fat_bootblock_check

; Read error: Block not found
fat_scan_error:
	clr	temp
	ret

fat_bootblock_check:

.if FAT16_DEBUG > 0
	printstring "fat16 bootblock check"
	printnewline
.endif

	ldiw	y,fat_vars
	ldiw	z,hostbuf

;   	get sectors per cluster from bootblock
	ldd	temp,z+FAT16_BSO_CLUSTSZ
	std	y+o_fat_clustersize,temp

.if FAT16_DEBUG > 0
	printstring "Sectors per Cluster "
	lcall	printhex
	printnewline
.endif

;	get max num of entries in root direktory from bootblock
	ldd	temp ,z+FAT16_BSO_NUMDIRENT
	ldd	temp2,z+FAT16_BSO_NUMDIRENT+1

.if FAT16_DEBUG > 0
	printstring "Max. entries in rootdir.: "
	lcall	printhexw
.endif

; Calculate begin of DATA Clusters within the Volume
; Num. Dir.Sektors = (Num. of Dir. entries * 32) / Bytes per Sektor

; Sectorsize is fixed at 512 bytes, makes 16 entries per sector


;   Num. Direntries / 16
	lsr	temp2
	ror	temp
	lsr	temp2
	ror	temp
	lsr	temp2
	ror	temp
	lsr	temp2
	ror	temp

	push	temp			;save # of rootdir sectors

.if FAT16_DEBUG > 0
	printstring "  ("
	movw	temp,_tmp0
	lcall	printhex
	printstring " sectors)"
	printnewline
.endif

.if FAT16_DEBUG > 1
; 	Print begin of Volume
	printstring "Begin of Volume at: "
	ldd	temp, y+o_fat_parttbl+2
	ldd	temp2,y+o_fat_parttbl+3
	lcall	printhexw
	ldd	temp, y+o_fat_parttbl+0
	ldd	temp2,y+o_fat_parttbl+1
	lcall	printhexw
	printnewline
.endif

;	get num of reseved sectors from bootblock
	ldd	_tmp0,z+FAT16_BSO_RESSECT
	ldd	_tmp1,z+FAT16_BSO_RESSECT+1

; 	Calculate begin of FAT within the Volume
	ldd	xl,y+o_fat_parttbl+0		
	ldd	xh,y+o_fat_parttbl+1
	ldd	temp,y+o_fat_parttbl+2
	ldd	temp2,y+o_fat_parttbl+3

	add	xl,_tmp0
	adc 	xh,_tmp1
	adc	temp,_0
	adc	temp2,_0

	std	y+o_fat_ptr2fat  ,xl
	std	y+o_fat_ptr2fat+1,xh
	std	y+o_fat_ptr2fat+2,temp
	std	y+o_fat_ptr2fat+3,temp2

.if FAT16_DEBUG > 1
	printstring "Begin of FAT at___: "
	lcall	printhexw
	movw	temp ,x
	lcall	printhexw
	printnewline
.endif

;	get num of sectors per FAT-Table from bootblock
	ldd	temp, z+FAT16_BSO_SECPERFAT
	ldd	temp2,z+FAT16_BSO_SECPERFAT+1

.if FAT16_DEBUG > 0
	printstring "Sectors per FAT___: "
	lcall	printhexw
	printnewline
.endif

;   	get num of FAT Tables from bootblock
	ldd	temp3,z+FAT16_BSO_NUMFATCP

.if FAT16_DEBUG > 0
	printstring "Ammount of FAT copies: "
	push	temp
	mov	temp,temp3
	lcall	printhex
	pop	temp
	printnewline
.endif

	movw	_tmp0,temp

; Calculate begin of Root- Directory within the Volume

	ldd	xl,   y+o_fat_ptr2fat+0
	ldd	xh,   y+o_fat_ptr2fat+1
	ldd	temp, y+o_fat_ptr2fat+2
	ldd	temp2,y+o_fat_ptr2fat+3

fat_calc_dp_loop:
	cp 	temp3,_0
	breq	fat_calc_dp_lend

	add	xl,_tmp0
	adc	xh,_tmp1
	adc	temp,_0
	adc	temp2,_0
	dec	temp3
	rjmp	fat_calc_dp_loop

fat_calc_dp_lend:

.if FAT16_DEBUG > 1
	printstring "Begin of DIR at___: "
	lcall	printhexw
	push	temp2
	push	temp
	movw	temp,x
	lcall	printhexw
	pop	temp
	pop	temp2
	printnewline
.endif

	pop	temp3			;number of rootdir sectors

	add	xl,temp3
	adc	xh,_0
	adc	temp,_0
	adc	temp2,_0

	std	y+o_fat_ptr2dat  ,xl
	std	y+o_fat_ptr2dat+1,xh
	std	y+o_fat_ptr2dat+2,temp
	std	y+o_fat_ptr2dat+3,temp2

.if FAT16_DEBUG > 1
	printstring "Begin of Data at__: "
	lcall	printhexw
	movw	temp ,x
	lcall	printhexw
	printnewline
.endif

;-------------------------------------------------------------------------------
; Here starts the scan of the directory for valid image files.


fat_next_sector_loop:

;   Get a pointer to the last+1 directory sector
	lds	xl,fat_ptr2dat
	lds	xh,fat_ptr2dat+1
	lds	yl,fat_ptr2dat+2
	lds	yh,fat_ptr2dat+3

;	Add actual offset
	sub	xl,temp3
	sbc	xh,_0
	sbc	yl,_0
	sbc	yh,_0

;  Load sector from Directory
	ldiw	z,hostbuf
	lcall	mmcReadSect
	tst	temp
	breq	fat_look_for_images

; Read error: Block not found
	clr	temp
	ret

; Looks at a read directory block for image entries
fat_look_for_images:
	
	ldiw	x,hostbuf

fat_look_for_loop:

	movw	y,x
	ldiw	z,fat_cfname*2
	ldi	temp2,STRLEN(FAT16_IMAGENAME_PREFIX)
	lcall	strncmp_p
	brne	fat_look_continue

	adiw	y,1
	adiw	z,1
	ldi	temp2,STRLEN(FAT16_IMAGENAME_SUFFIX)
	lcall	strncmp_p
	brne	fat_look_continue

	movw	y,x
	ldd	temp2,y+STRLEN(FAT16_IMAGENAME_PREFIX)
	ldi	temp,FAT16_FIRST_IMAGENAME
fat_look_imgname_loop:
	cp	temp,temp2
	breq	fat_look_imgname_match
	inc	temp
	cpi	temp,FAT16_LAST_IMAGENAME+1
	brlo	fat_look_imgname_loop
	rjmp	fat_look_continue

fat_look_imgname_match:
	rcall	fat_store_new_entry
	movw	x,y

fat_look_continue:

	adiw	x,32
	ldi	temp,high(hostbuf+HOSTSIZE)
	cpi	xl,low(hostbuf+HOSTSIZE)
	cpc	xh,temp
	brne	fat_look_for_loop

fat_scan_next_sector:
	
	dec	temp3
	brne	fat_next_sector_loop

	ret



#if 0
dbg_print_parttbl_raw:
	push	zh
	push	zl
	push	temp2
	push	temp
	ldiw	z,hostparttbl
dbg_pptblraw_lp:
	lcall	dbg_hexdump_line
	adiw	z,PARTENTRY_SIZE
	cpi	zl,low(hostparttbltop)
	ldi	temp,high(hostparttbltop)
	cpc	zh,temp
	brne	dbg_pptblraw_lp
	pop	temp
	pop	temp2
	pop	zl
	pop	zh
	ret
#endif

;-------------------------------------------------------------------------------
;	Create new Partition Entry
; ============================================================================
; Function: Create new Partition Entry
; ============================================================================
; Parameters:	[in]	yh,yl		Directory entry
;
; ----------------------------------------------------------------------------
; Registers  : 	
; Variables  : 	[used]
;		[changes] temp
; ----------------------------------------------------------------------------
; Description:
; 
; ============================================================================

fat_store_new_entry:

;   Found a valid image
.if FAT16_DEBUG > 1
	printnewline
	printstring "Found a valid Image! Y="
	push	temp2
	push	temp
	movw	temp ,y
	lcall	printhexw
	pop	temp
	pop	temp2
	printnewline
.endif

#if 0
;;;;
	printstring "Insert entry in Tab: --> "
	mov	temp,temp2
	lcall	uartputc
	printnewline
	printstring "Tab before:"

	rcall	dbg_print_parttbl_raw
	printnewline
;;;;
#endif
	ldiw	z,hostparttbl

fat_st_searchpos:
	ldd	temp,z+PTAB_TYPE
	tst	temp
	breq	fat_st_insert_slot			
	sbrs	temp,log2(dskType_FAT)
	rjmp	fat_st_search_next

	ldd	temp,z+PTAB_START+2
	cp	temp2,temp
	brlo	fat_st_ins_before

fat_st_search_next:
	adiw	z,PARTENTRY_SIZE
	cpi	zl,low(hostparttbltop)
	ldi	temp,high(hostparttbltop)
	cpc	zh,temp
	brne	fat_st_searchpos

; Table is full.
#if 0
;;;;
	push	temp2
	push	temp
	printstring "Table is full.   --> ptr: "
	movw	temp,z
	lcall	printhexw
	printnewline
	pop	temp
	pop	temp2
;;;;
#endif
	ret

fat_st_ins_before:
	movw	x,z
	ldiw	z,hostparttbltop-PARTENTRY_SIZE
fat_st_insert_mkslotloop:
	cp	zl,xl
	cpc	zh,xh
	breq	fat_st_insert_slot

	ld	_tmp0,-z
	std	z+PARTENTRY_SIZE,_tmp0
	rjmp	fat_st_insert_mkslotloop

fat_st_insert_slot:

;   Set Type of Partition to FAT16- Fileimage
	ldi 	temp,dskType_FAT
	std	z+PTAB_TYPE,temp

;   Offset to Startcluster + 2
	ldd	_tmp0,y+0x1A
	ldd	_tmp1,y+0x1B
	std	z+PTAB_START,  _tmp0
	std	z+PTAB_START+1,_tmp1	

	std	z+PTAB_START+2,temp2
	std	z+PTAB_START+3,_0

;   Convert Filesize to ammount of sectors
;   (calc with 512byte/sector)
	ldd	_tmp0,y+0x1C
	ldd	xl,   y+0x1D
	ldd	xh,   y+0x1E
	ldd	temp, y+0x1F

	clc
	cpse	_tmp0,_0		;round up
	adiw	x,1
	adc	temp,_0

	lsr	temp
	ror 	xh
	ror 	xl

	adc	xl,_0
	adc	xh,_0
	adc	temp,_0

;   store ammount of sectors in partitiontable	

	tst	temp			;file size larger than 65535 sectors?
	breq	fat_add_noprune
	
	ldi	xl,255
	ldi	xh,255
fat_add_noprune:
	std	z+PTAB_SIZE,  xl
	std	z+PTAB_SIZE+1,xh

.if FAT16_DEBUG > 1
; Test finding of the first sector

	push	yh
	push	yl
	ldd	xl,z+1
	ldd	xh,z+2

	ldiw	y,fat_vars
	rcall	fat_gethostsec

	printstring "Begin of image at: "
	lcall	printhexw
	movw	temp ,x
	lcall	printhexw
	printnewline
	pop	yl
	pop	yh
.endif
		

; Table counts one more entry if it was'nt allready full

	lds	temp,ndisks
	cpi	temp,MAXDISKS
	breq	fat_add_nomore
	inc	temp
	sts	ndisks,temp

fat_add_nomore:

#if 0
;;;;
	printnewline
	printstring "Table now:"
	rcall	dbg_print_parttbl_raw
	printnewline
;;;;
#endif
	ret


; ============================================================================
; Function: Cluster to HostSector 
; ============================================================================
; Registers:	[in]	xl,xh			Cluster Number
;		[out]	yh,yl,xh,xl		Sector Number on Disk
; Variables:	[in]	fat_clustersize		Ammount of Sectors per Cluster
;		[out]	temp			=0
; ----------------------------------------------------------------------------
; Description:
; Calculates the logical Sectornumber given the physical ClusterNumber
; and the size of a Cluster un sectors.
;
; ! Only works with Clustersizes 2,4,8,16,32,64,128 !
; ============================================================================
fat_gethostsec:

; Get Offset into Data area of Disk

	clr 	_tmp0
	clr	_tmp1
	sbiw	x,2		; Substract the 2 reserved clusters

	ldd	temp,y+o_fat_clustersize
	lsr	temp
fat_c2s_loop:
	tst	temp
	breq	fat_c2s_end
	lsr	temp

	lsl	xl
	rol	xh
	rol	_tmp0
	rol	_tmp1
	rjmp	fat_c2s_loop
fat_c2s_end:

; Add begin of data area to offset

	ldd	temp,y+o_fat_ptr2dat+0
	add	xl,temp
	ldd	temp,y+o_fat_ptr2dat+1
	adc	xh,temp
	ldd	temp,y+o_fat_ptr2dat+2
	adc	_tmp0,temp
	ldd	temp,y+o_fat_ptr2dat+3
	adc	_tmp1,temp
	movw	temp,_tmp0

	ret

; ====================================================================
; Function: Searches a physical Cluster, given the logical Cluster
; ====================================================================
; Registers:	[in]	xh:xl		logical- Cluster 
;		[out]	temp2:temp	physical- Cluster
; Variables:
; --------------------------------------------------------------------
; Description:
; ====================================================================

fat_find_phsy_clust:

	lds	zl,hostdsk
	rcall 	dsk_getpartentry	; get partition entry

.if FAT16_DBG_FAT > 0
	printstring "Search log. cluster "
	movw	temp,x
	lcall	printhexw
	printnewline
.endif
		
; Get First FAT- Cluster Number of Diskimage
	
	ldd	temp, z+1
	ldd	temp2,z+2

.if FAT16_DBG_FAT > 0
	printstring "Search phys. cluster "
	lcall	printhexw
	printnewline
.endif

fat_next_phsy_clust:	
	cp	xl,_0
	cpc	xh,_0
	breq	fat_found_phsy_clust

;	Get Next Cluster from Fat

; Trick: 512 Bytes Per Sector equals to 256 FAT- entries per Sector
; so given:  temp is the Offset within the FAT Sector
;            temp2 is the number off the FAT sector to Read 

	push 	xl
	push 	xh

; Create FAT Offset Value

	clr	zh
	mov	zl,temp
	lsl	zl
	rol 	zh

.if FAT16_FATBUF

; Check, if required fat sector allready in buffer

	ldd	temp,y+o_fat_last_fatsect
	std	y+o_fat_last_fatsect,temp2
	cp	temp,temp2
	breq	fat_phys_1

; Not in buffer, get fat sector

	ldd 	xl,   y+o_fat_ptr2fat		;get FAT start
	ldd 	xh,   y+o_fat_ptr2fat+1
	ldd 	_tmp0,y+o_fat_ptr2fat+2
	ldd 	_tmp1,y+o_fat_ptr2fat+3
	add	xl,temp2			;add cluster offset within sector
	adc	xh,_0
	adc 	_tmp0,_0
	adc 	_tmp1,_0
	movw	y,_tmp0
.if 0
	printnewline
	printstring "Read FAT sec: "
	mov	temp,temp2
	lcall	printhex
	printstring ",  "
	movw	temp,z
	lcall	printhexw
	printstring " "
.endif
	push 	zl
	push 	zh
	ldiw	z,fat_buf

; in	zh,zl		Pointer to Word within the Sector to read	
; in	yh..xl		Start sector number (LBA)
; out	zh,zl		word thats been read
	lcall	mmcReadSect
	pop 	zh
	pop 	zl

fat_phys_1:
	ldiw	y,fat_buf
	add	zl,yl
	adc	zh,yh
	ldd	temp, z+0
	ldd	temp2,z+1
	ldiw	y,fat_vars

.else

	ldd 	xl,y+o_fat_ptr2fat			;get FAT start
	ldd 	xh,y+o_fat_ptr2fat+1
	ldd 	_tmp0,y+o_fat_ptr2fat+2
	ldd 	_tmp1,y+o_fat_ptr2fat+3
	movw	y,_tmp0
	add	xl,temp2			;add cluster offset within sector
	adc	xh,_0
	adc 	yl,_0
	adc 	yh,_0
	lcall 	mmcReadWord
	movw 	temp,z
	ldiw	y,fat_vars

.endif	/* FAT16_FATBUF */

	pop 	xh
	pop 	xl


;	Check next logical Cluster

	sbiw	x,1
	rjmp	fat_next_phsy_clust
	
; Found the physical cluster
fat_found_phsy_clust:
	
.if FAT16_DBG_FAT > 0
	printstring "Found phys. Cluster at:"
	lcall	printhexw
	printnewline
.endif	

	ret

; ============================================================================
; Function: This Routine searches for the Sector within an Imagefile 
; ============================================================================
; Registers:	[out] xl,xh,yl,yh	Pointer to the sector on the SD-Card
;		[out] temp		Error variable (0= No Error)
; Variables:	[in] hostdsk		host disk #,  (partition #)
; 		[in] hostlba		host block #, relative to part.start
;		[in] fat_last_dsk	number of disk with entry in cache
;		[in] fat_log_clust	last searched logical cluster
;		[in] fat_clust_offset	offset within the cluster
;               [in] fat_clust_ptr	sector of last real cluster
; ----------------------------------------------------------------------------
; Description:
; This routine uses the variables hostdsk and hostlba to find an sector
; in the imagefile.
; The CP/M sector given within "hostlba" are splited to a logical cluster-
; number and the subsector within this logical cluster.
; logical cluster number = hostlba / fat_clustersize
; The logical cluster number will be compared to the logical cluster number
; within the cache. When this clusters are the same and the diskid's are
; also the same, then the cached physical sector will be used.
; When the clusters or the disks don't match, a seek for the physical
; cluster is performed. This seek is done thru an access over the fat of
; the fat16 partition. the routine starts at the first cluster of the 
; imagefile and goes along the linked list of clusternumber till it reaches
; the searched cluster. The found clusternumber will be used to calculate
; the sektor where this cluster lies on the sd card. Both the found physical
; cluster and the logical cluster together with the physical sectornumber
; are stored in the cache.
; The last step done is to add the subsectoroffset to the found physical
; sector. this gives the pointer to the sector to be read and or written.
; ============================================================================

fat_lba_to_phys:

; ################# Get logical Number of Cluster within the imagefile
;	printstring "calc log sector"
; Logical Sectornumber in x

	ldiw	y,fat_vars

	movw	temp,x
	mov 	_tmp0,_0
	mov 	_tmp1,_0

; Divide logical sectornumber by size of cluster in sectors

	ldd	zl,y+o_fat_clustersize
	lsr     zl
fat_search_clst_lp:
	tst	zl
	breq	fat_found_clst

	lsr	_tmp1
	ror	_tmp0
	ror	xh
	ror	xl
	
	lsr	zl

	rjmp	fat_search_clst_lp
		
; at this point xh and xl are carying the logical cluster number
;	printstring "find subsector"
; ################# Get subsector within the logical cluster for later use

fat_found_clst:			
	mov	_tmp0,xl
	ldd	zl,y+o_fat_clustersize
	lsr	zl
fat_search_clst_lp2:
	tst	zl
	breq	fat_found_subsec
	lsl	_tmp0

	lsr	zl
	rjmp	fat_search_clst_lp2		

fat_found_subsec:
	mov	zl,temp
	sub	zl,_tmp0
	std	y+o_fat_clust_offset,zl

; Check against last HOSTDISK searched
	lds	_tmp0,hostdsk
	ldd	_tmp1,y+o_fat_last_dsk
	std	y+o_fat_last_dsk,_tmp0
	cp	_tmp0,_tmp1
	brne	fat_wrong_cache_clst

; Check against last Cluster searched
	ldd	_tmp0,y+o_fat_log_clust
	ldd	_tmp1,y+o_fat_log_clust+1

	cp	_tmp0,xl
	cpc	_tmp1,xh
	brne	fat_wrong_cache_clst

; Last Cluster = searched Cluster -> get Sectornumber from cache
	ldd	xl,   y+o_fat_clust_ptr
	ldd	xh,   y+o_fat_clust_ptr+1
	ldd	temp, y+o_fat_clust_ptr+2
	ldd	temp2,y+o_fat_clust_ptr+3

	rjmp	fat_add_offset

;  Cluster is not in cache, so we must search for it
fat_wrong_cache_clst:
	std	y+o_fat_log_clust,xl
	std	y+o_fat_log_clust+1,xh

;  Map Logical Cluster-Number to "Physical" Cluster Number using the FAT

	rcall   fat_find_phsy_clust

;  Get StartSector of "physical" Cluster

	movw 	x,temp
	rcall   fat_gethostsec

; Found the physical sector

.if FAT16_DBG_FAT > 0
	printstring "Found phys. Sector at:"
	push	temp2
	push	temp
	lcall	printhexw
	movw	temp,x
	lcall	printhexw
	printnewline
	pop	temp
	pop	temp2
.endif	

;   Save the found Sector for later use into cache

	std	y+o_fat_clust_ptr  ,xl
	std	y+o_fat_clust_ptr+1,xh
	std	y+o_fat_clust_ptr+2,temp
	std	y+o_fat_clust_ptr+3,temp2

;   Add- Subsector to Startsector 
fat_add_offset:
	ldd	zl,y+o_fat_clust_offset
	add	xl,zl
	adc	xh,_0
	adc	temp,_0
	adc	temp2,_0
	movw	y,temp

; Found the physical sector
.if FAT16_DBG_FAT > 0
	printstring "Sector with Offset at:"
	movw	temp,y
	lcall	printhexw
	movw	temp,x
	lcall	printhexw
	printnewline
.endif

	ret

#endif /* FAT16_SUPPORT */

; vim:set ts=8 noet nowrap


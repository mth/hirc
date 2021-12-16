import strutils
import os
import posix
import tables

let who = paramStr(1)
let nick = paramStr(2)
let datFile = if paramCount() < 3: "seen.dat"
              else: paramStr(3)

let seenFile = try: datFile.open
               except IOError as e:
                echo "Pole siin kedagi."
                quit e.msg

var seen = initTable[string, seq[string]]()
var count = 0
var line: string

# each line is "nick\tTIME\tMESSAGE", online nicks have +TIME
while seenFile.readLine(line):
    inc count
    var r = line.split('\t')
    if r.len <= 1: # Connected IRC, therefore all previous seens are offline
        for v in seen.mvalues:
            v[1].removePrefix '+'
    else:
        let key = r[0].toLowerAscii
        if r[2].len == 0 and key in seen: # steels message
            r[2] = seen[key][2]
        seen[key] = r
seenFile.close

# write compacted seen.dat
# this is because the main bot just appends to the seen.dat
if seen.len < count:
    let tmpName = datFile & ".tmp"
    let tmpFile = tmpName.open fmAppend # fmAppend to not truncate before locking
    var lock = Tflock(l_type: static[cshort](F_WRLCK))
    let fd = tmpFile.getOsFileHandle
    if fcntl(fd, F_SETLK, addr lock) != -1:
        # now that we have lock, ensure that the file is empty
        discard ftruncate(fd, 0)
        for entry in seen.values:
            tmpFile.writeLine entry.join("\t")
        tmpFile.flushFile
        if fdatasync(fd) == 0:
            moveFile tmpName, datFile
        else:
            discard tryRemoveFile tmpName
        tmpFile.close
    else:
        tmpFile.close

let lowerNick = nick.toLowerAscii
if who.toLowerAscii == lowerNick:
    echo nick & ": Paraku oled sa ikka veel siin"
    quit 0

let r = seen.getOrDefault lowerNick
if r == []:
    echo "Ei tea midagi " & nick & "'st"
elif r[1].startsWith '+':
    echo r[0] & " on praegu kanalil"
else:
    echo r[0] & " oli siin kunagi"


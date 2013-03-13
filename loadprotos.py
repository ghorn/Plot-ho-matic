import mhempc_pb2
import time

def loadProtos(filename):
    sizesfilename = filename+".sizes"
    print 'reading sizes file "'+sizesfilename+'"...'
    f = open(sizesfilename, 'r')
    sizes = []
    for line in f:
        sizes.append(int(line.strip()))
    f.close()
    
    print 'reading from "'+filename+'"...'
    f = open(filename, 'r')
    raw = []
    t0 = time.time()
    for size in sizes:
        raw.append( f.read(size) )
    dt = time.time() - t0
    f.close()
    print "read %d bytes in %.3f seconds, %.3g bytes/s\n" % (sum(sizes), dt, sum(sizes)/dt)
    
    print 'deserializing protos...'
    protos = []
    t0 = time.time()
    for r in raw:
        proto = mhempc_pb2.MheMpcHorizons()
        proto.ParseFromString( r )
        protos.append( proto )
    dt = time.time() - t0
    print "desearialized %d protos in %.2f seconds, %.3g bytes/s" % (len(protos), dt, sum(sizes)/dt)

    return protos

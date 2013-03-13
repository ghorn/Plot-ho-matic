import matplotlib.pyplot as plt
from loadprotos import loadProtos

protos = loadProtos("mhe-mpc-horizons_log.dat")

plt.figure()
plt.plot( [p.currentState.diffStates.z for p in protos] )
plt.title('z')
plt.show()


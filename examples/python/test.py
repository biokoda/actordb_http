# requires msgpack-python and requests
# pip install msgpack-python
# pip install requests
#
import msgpack
import requests

r = requests.get('http://localhost:33380/v1/_db/', auth=('root', 'rootpass'), headers={'accept':'Application/x-msgpack'})
print msgpack.unpackb(r.content)

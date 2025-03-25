from google.transit import gtfs_realtime_pb2
from google.protobuf.json_format import MessageToDict, MessageToJson
import requests
import json

def get_mta_alerts():

  url = 'https://api-endpoint.mta.info/Dataservice/mtagtfsfeeds/camsys%2Fsubway-alerts'
  feed = gtfs_realtime_pb2.FeedMessage()
  response = requests.get(url)
  feed.ParseFromString(response.content)
  
  for entity in feed.entity:
      if entity.HasField('trip_update'):
          print(entity.trip_update)
  
  entity_list = [MessageToDict(entity) for entity in feed.entity]
  entity_json = json.dumps(entity_list)
  
  return entity_json


from models import user_profile, UserProfile
from utils import find_matches_within_radius
from copy import copy

user_profile2 = copy(user_profile)
user_profile2.oid = 'some other id'
user_profile2.name = "German 2"
user_profile2.dob = '30/05/2003'
user_profile2.location = [26.8674446, 81.107758]
user_profile2.gender = "female"
user_profile2.preference = 'male'

user_profile3 = copy(user_profile)
user_profile3.oid = 'some other other id'
user_profile3.name = "German 2"
user_profile3.dob = '30/05/2003'
user_profile3.location = [26.8674466, 81.109748]
user_profile3.gender = "female"
user_profile3.preference = 'male'

matches = find_matches_within_radius(user_profile, [user_profile, user_profile2, user_profile3])
assert matches == [user_profile2.oid, user_profile3.oid]


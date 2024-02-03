# Function to calculate the Haversine distance between two points
# Source: https://louwersj.medium.com/calculate-geographic-distances-in-python-with-the-haversine-method-ed99b41ff04b
import datetime
from dateutil import relativedelta
from math import radians, sin, cos, sqrt, atan2
from models import UserProfile


def haversine_distance(lat1, lon1, lat2, lon2):
    R = 6371  # Earth radius in kilometers

    dlat = radians(lat2 - lat1)
    dlon = radians(lon2 - lon1)

    a = sin(dlat / 2) ** 2 + cos(radians(lat1)) * cos(radians(lat2)) * sin(dlon / 2) ** 2
    c = 2 * atan2(sqrt(a), sqrt(1 - a))

    distance = R * c
    return distance


def current_age(dob_str: str) -> int:
    dob = datetime.datetime.strptime(dob_str, '%d/%m/%Y')
    today = datetime.date.today()
    age = relativedelta.relativedelta(today, dob).years
    return age


def find_matches_within_radius(user: UserProfile, user_profiles: list[UserProfile]):
    if len(user_profiles) == 0:
        return []

    matches_found = []
    max_distance = user.search_radius

    for other_user in user_profiles:
        if user.oid != other_user.oid:
            distance = haversine_distance(
                user.location[0], user.location[1], other_user.location[0], other_user.location[1]
            )

            user_age = current_age(other_user.dob)
            if distance <= max_distance and user.preference == other_user.gender and other_user.oid not in user.skips\
                    and user.age_range[0] <= user_age <= user.age_range[1]\
                    and other_user not in user.selections:
                matches_found.append(other_user)

    return list(map(lambda u: u.oid, matches_found))

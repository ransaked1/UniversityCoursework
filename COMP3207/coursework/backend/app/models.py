"""
Models module.

This module provides all the schema models and their types in the form of pydantic models
"""

from datetime import datetime

from pydantic import (
    BaseModel,
    field_validator,
    Field,
    EmailStr
)
from typing import Optional, List

@field_validator('location')
@classmethod
def validate_location(cls, location: Optional[List[float]]):
    if location is None:
        return None
    if len(location) != 2:
        raise ValueError('Invalid location format')
    if location[0] < -90 or location[0] > 90:
        raise ValueError('Invalid latitude')

    if location[1] < -190 or location[1] > 180:
        raise ValueError('Invalid longitude')
    return location

@field_validator('age_range')
@classmethod
def validate_age(cls, age_range: Optional[List[int]]):
    if age_range is None:
        return None
    if len(age_range) != 2:
        raise ValueError('Invalid age range format')

    if age_range[0] <= 18:
        raise ValueError('Age range start must be above or equal to 18')
    return age_range

class Token(BaseModel):
    access_token: str
    token_type: str


class CatData(BaseModel):
    """Cat data model"""
    name: str
    age: int
    breed: str
    sex: bool  # false = male, true = female
    bio: str
    image_ids: List[str] = Field([], min_length=0, max_length=5)  # list of image ids


class UserData(BaseModel):
    email: EmailStr 
    dob: str  # date of birth
    gender: str = Field(..., min_length=1, max_length=100)
    name: str = Field(..., min_length=1, max_length=100)
    surname: str = Field(..., min_length=1, max_length=100)
    bio: str = Field(default='', min_length=0, max_length=300)
    preference: str = Field(..., min_length=0, max_length=100)  # gender of the other owner preference
    age_range: List[int]  # age range [min, max] inclusive
    location: List[float]  # "lat, lon" Used to find matches nearby
    profile_pic_url: str
    cat: CatData  # User's cat profile

    @field_validator('dob')
    @classmethod
    def validate_dob(cls, dob: str):
        today = datetime.today()
        dob_parsed = datetime.strptime(dob, '%d/%m/%Y')
        age = today.year - dob_parsed.year - ((today.month, today.day) < (dob_parsed.month, dob_parsed.day))
    
        if age < 18:
            raise ValueError("User must be at least 18 years old")
    
        return dob


class CatPatch(BaseModel):
    """Cat patch model"""
    name: str = Field(default=None, min_length=1, max_length=100)
    age: int = Field(default=None, gt=0)
    breed: str = Field(default=None, min_length=1, max_length=100)
    sex: bool = Field(default=None, min_length=0, max_length=100)
    bio: str = Field(default=None, min_length=0, max_length=300)
    image_ids: List[str] = Field(None, min_length=0, max_length=5)


# This model can assign attributes to default None as it allows for
# partial patching of db entries
class UserPatch(BaseModel):
    """User patch model"""
    email: Optional[EmailStr] = None
    gender: Optional[str] = None
    name: Optional[str] = Field(default = None, min_length=1, max_length=100)
    surname: Optional[str] = Field(default = None, min_length=1, max_length=100)
    bio: Optional[str] = Field(default = None, min_length=0, max_length=100)
    location: Optional[List[float]] = None
    age_range: Optional[List[int]] = None
    profile_pic_url: Optional[str] = None
    cat: Optional[CatPatch] = None
    preference: Optional[str] = None
    search_radius: Optional[float] = Field(default=None, gt=0, lt=100)

class CatProfile(CatData):
    """Cat profile that is returned separately from the owner"""
    owner_id: str


class LoginCredentials(BaseModel):
    email: EmailStr
    password: str


class UserProfile(UserData):
    oid: str
    hashed_password: str                    # Hashed password
    matches: List[str] = []     # List of IDs of matches
    matches_allowed: int = 3                # Number of matches allowed
    selections: List[str] = []  # Profiles that a user selected
    skips: List[str] = []
    potentials: List[str] = []  # List of profiles nearby
    search_radius: float = Field(default=10.0, gt=0, lt=100)  # Search radius in km, default is 10.0


class RegisterUser(UserData):
    password: str = Field(default='', min_length=6, max_length=16)


class Message(BaseModel):
    from_u: str
    datetime: str
    message: str


class Match(BaseModel):
    oid: str
    user_1: str
    user_2: str
    meeting_confirmation: list[str] = []  # list of confirmation who confirmed the meeting
    messages: list[Message] = []


class MeetingConfirmation(BaseModel):
    picture_url: str


class ConfirmSuggestion(BaseModel):
    oid: str


class SkipSuggestion(BaseModel):
    oid: str


class ConfirmResponse(BaseModel):
    """
    Represents a confirmation of matching
    `match_id` is only set if match is bidirectional
    """
    matches_left: int
    match_id: str | None


fake_cat = CatProfile(
    owner_id="0",
    name="Fnuffy",
    age=11,
    breed="N/A",
    sex=False,
    bio="He loves flowers",
    image_ids=['id_1']
)

user_profile = UserProfile(
    oid='some id',
    name="German",
    surname="test",
    bio='I want to find love of my life',
    hashed_password="H1H12D",
    age=20,
    email='gn2g21@soton.ac.uk',
    dob='30/05/2003',
    location=[26.7674446, 81.109758],
    age_range=[18, 25],
    gender="male",
    preference='female',
    matches=["0", "1", "2"],
    matches_allowed=3,
    selections=["73", "22"],
    potentials=["9", "7"],
    skips=[],
    search_radius=12.2,
    cat=fake_cat,
    profile_pic_url="test"
)

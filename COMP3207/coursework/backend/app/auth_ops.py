"""
Authentication operations  module.

This module contains authentication logic, password hashing, and JWT/JSON token handling.
"""


from datetime import datetime, timedelta

from fastapi import Security, HTTPException
from jose import JWTError, jwt
from fastapi.security import OAuth2PasswordBearer, OAuth2PasswordRequestForm
from pydantic import ValidationError
from starlette import status

from models import LoginCredentials, Token, RegisterUser, UserProfile
from passlib.context import CryptContext
from database import Database
import os
import re

SECRET_KEY_TOKEN = os.environ['SECRET_KEY_TOKEN']
ALGORITHM = "HS256"
ACCESS_TOKEN_EXPIRE_MINUTES = 120
pwd_context = CryptContext(schemes=["bcrypt"], deprecated="auto")
oauth2_scheme = OAuth2PasswordBearer(tokenUrl="/profiles/token")
user_db = Database()


def create_access_token(data: dict, expires_delta: timedelta = None):
    to_encode = data.copy()

    if expires_delta:
        expire = datetime.now() + expires_delta
    else:
        expire = datetime.now() + timedelta(minutes=ACCESS_TOKEN_EXPIRE_MINUTES)

    to_encode.update({"exp": expire})
    encoded_jwt = jwt.encode(to_encode, SECRET_KEY_TOKEN, algorithm=ALGORITHM)

    return encoded_jwt


async def authenticate_user(login_credentials: LoginCredentials):
    user = get_user_by_email(login_credentials.email)

    # Return 'None' instead of False as per fastAPI docs
    if not user:
        print("Could not retrieve email")
        return None

    # the password retrieved here from database is hashed, and gets verified
    if not pwd_context.verify(login_credentials.password, user.hashed_password):
        print("Password incorrect")
        return None

    return user


def get_user_by_id(user_id: str):
    user_dict = user_db.get_user_by_id(user_id)

    if user_dict:
        return UserProfile(**user_dict)
    else:
        return None


def get_user_by_email(email: str) -> UserProfile | None:
    user_dict = user_db.get_user_by_email(email)

    if user_dict:
        return UserProfile(**user_dict)
    else:
        return None


# Once the user is provided a token, use this function to access routes with user authentication
async def get_current_user(token: str = Security(oauth2_scheme)) -> UserProfile:
    credentials_exception = HTTPException(
        status_code=status.HTTP_401_UNAUTHORIZED,
        detail="Could not validate credentials",
        headers={"WWW-Authenticate": "Bearer"},
    )

    try:
        payload = jwt.decode(token, SECRET_KEY_TOKEN, algorithms=[ALGORITHM])
        user_id: str = payload.get("sub")
        if user_id is None:
            raise credentials_exception
    except (JWTError, ValidationError):
        raise credentials_exception

    user = get_user_by_id(user_id)
    if user is None:
        raise credentials_exception

    return user

def validate_password(password):
    # At least 1 digit, At least 1 lower, At least 1 upper, At least 1 special, At least 8 chars, No white space
    return bool(re.match(r"(?=.*[0-9])(?=.*[a-z])(?=.*[A-Z])(?=.*[@#$%^!*&+=])(?=\S+$).{8,}", password))

def pass_matches(password, confirm):
    return password == confirm

def validate_email(email):
    return bool(re.match(r"[^@]+@[^@]+\.[^@]+", email))

def validate_dob(dob):
    dob_day = int(dob.split("-")[2])
    dob_month = int(dob.split("-")[1])
    dob_year = int(dob.split("-")[0])
    
    today = datetime.today()
    age = today.year - dob_year - ((today.month, today.day) < (dob_month, dob_day))

    return age >= 18

def validate_cat_age(age):
    return age > 0

def no_empty_fields(user_data):
    fields = {
        'email': user_data.email,
        'password': user_data.password,
        'name': user_data.name,
        'surname': user_data.surname,
        'dob': user_data.dob,
        'bio': user_data.bio,
        'cat name': user_data.cat.name,
        'cat age': user_data.cat.age,
        'cat breed': user_data.cat.breed,
        'cat bio': user_data.cat.bio
    }

    for field_name, field_value in fields.items():
        if not field_value:
            return False, field_name

    return True, None

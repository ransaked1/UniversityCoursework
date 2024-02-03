"""
Profile matching module.

"""
from math import radians, cos, sin, atan2, sqrt

from bson import ObjectId
from fastapi import APIRouter, HTTPException, File, UploadFile, status, Depends, Query
from typing_extensions import Annotated

from auth_ops import get_current_user
from database import Database
from models import Match, Message, MeetingConfirmation, ConfirmResponse, ConfirmSuggestion, UserProfile, fake_cat, \
    CatProfile
from typing import Any
from profile import get_cat_profile, get_user_profile

router = APIRouter(prefix="/match", tags=["match"])
db = Database()


@router.get("/suggest", status_code=status.HTTP_200_OK)
async def get_suggestion(current_user: UserProfile = Depends(get_current_user)) -> CatProfile | None:
    """
    Get next matched cat profile for user
    :param current_user: authenticated user
    :return: matching cat's profile
    """

    if current_user.matches_allowed == 0:
        raise HTTPException(status_code=status.HTTP_403_FORBIDDEN, detail='You have reached your matches limit. '
                                                                          'Go meet someone')

    if len(current_user.potentials) == 0:
        return None
    suggestion = current_user.potentials[-1]
    cat_profile = await get_cat_profile(suggestion, current_user)

    return cat_profile


@router.post("/confirm", status_code=status.HTTP_200_OK)
async def confirm_suggestion(confirmation: ConfirmSuggestion,
                             current_user: UserProfile = Depends(get_current_user)) -> ConfirmResponse:
    """
    Confirm that the user liked the suggestion
    :param confirmation: the confirmation of choice details
    :param current_user: authenticated user
    :return: confirmation response with details for the date
    """
    user = db.get_user_by_id(confirmation.oid)
    if user is None:
        raise HTTPException(status_code=status.HTTP_404_NOT_FOUND, detail="Not found")

    user = UserProfile(**user)
    if current_user.oid in user.selections:
        match = Match(
            oid=str(ObjectId()),
            user_1=current_user.oid,
            user_2=user.oid,
            meeting_confirmation=list(),
            messages=list()
        )
        match_id = db.create_match(match.model_dump())
        if match_id is None:
            raise HTTPException(status_code=status.HTTP_500_INTERNAL_SERVER_ERROR, detail='Error creating a match')

        # ------- Update other user ------

        other_user_matches_allowed = user.matches_allowed
        other_user_matches_allowed -= 1

        other_user_matches = user.matches
        other_user_matches.append(match_id)
        db.update_user(user.oid, {"matches_allowed": other_user_matches_allowed,
                                  "matches": other_user_matches})

        # ------ Update current user ----

        current_user_matches_allowed = current_user.matches_allowed
        current_user_matches_allowed -= 1

        current_user_matches = current_user.matches
        current_user_matches.append(match_id)

        current_user_selections = current_user.selections
        current_user_selections.append(user.oid)

        current_user_potentials = current_user.potentials
        current_user_potentials.remove(user.oid)
        db.update_user(current_user.oid, {"selections": current_user_selections,
                                          "matches_allowed": current_user_matches_allowed,
                                          "matches": current_user_matches,
                                          "potentials": current_user_potentials})

        return ConfirmResponse(
            match_id=match_id,
            matches_left=current_user_matches_allowed
        )
    else:
        current_user_selections = current_user.selections
        current_user_selections.append(user.oid)

        current_user_potentials = current_user.potentials
        current_user_potentials.remove(user.oid)
        db.update_user(current_user.oid, {"selections": current_user_selections,
                                          "potentials": current_user_potentials})

    return ConfirmResponse(
        match_id=None,
        matches_left=current_user.matches_allowed
    )


@router.post("/skip", status_code=status.HTTP_200_OK)
async def skip_suggestion(confirmation: ConfirmSuggestion,
                          current_user: UserProfile = Depends(get_current_user)) -> ConfirmResponse:
    """Skips the given profile"""
    user = db.get_user_by_id(confirmation.oid)
    if user is None:
        raise HTTPException(status_code=status.HTTP_404_NOT_FOUND, detail="Not found")

    user = UserProfile(**user)
    current_user_skips = current_user.skips
    current_user_skips.append(user.oid)

    current_user_potentials = current_user.potentials
    current_user_potentials.remove(user.oid)
    db.update_user(current_user.oid, {"skips": current_user_skips,
                                      "potentials": current_user_potentials})

    return ConfirmResponse(
        match_id=None,
        matches_left=current_user.matches_allowed
    )


@router.get("/{mid}", status_code=status.HTTP_200_OK, response_model=Match, response_model_exclude_unset=True)
async def get_match(mid: str,
                    current_user: UserProfile = Depends(get_current_user)) -> Match | None:
    match = db.get_match(mid)
    if match is None:
        raise HTTPException(status_code=status.HTTP_404_NOT_FOUND, detail="Not found")
    match['messages'] = []
    match = Match(**match)
    if current_user.oid != match.user_1 and current_user.oid != match.user_2:
        raise HTTPException(status_code=status.HTTP_403_FORBIDDEN,
                            detail='You do not have permission to request this match data')

    return match


@router.get("/{mid}/messages", status_code=status.HTTP_200_OK)
async def get_messages(mid: str,
                       size: Annotated[str | None, Query(regex="^[0-9]*[1-9][0-9]*$")] = None,
                       current_user: UserProfile = Depends(get_current_user)) -> list[Message]:
    if size is not None:
        try:
            list_len = int(size)
        except ValueError:
            raise HTTPException(status_code=status.HTTP_400_BAD_REQUEST, detail="Invalid length list")
    else:
        list_len = None

    match = db.get_match(mid)
    messages = match['messages']
    match['messages'] = list(map(lambda d: Message(**d), messages))
    if match is None:
        raise HTTPException(status_code=status.HTTP_404_NOT_FOUND, detail="Not found")
    match = Match(**match)
    if current_user.oid != match.user_1 and current_user.oid != match.user_2:
        raise HTTPException(status_code=status.HTTP_403_FORBIDDEN,
                            detail='You do not have permission to request this match data')
    if list_len is None:
        return match.messages
    else:
        return match.messages[-list_len:]


@router.post("/{mid}/messages", status_code=status.HTTP_201_CREATED)
async def post_message(mid: str, msgs: list[Message], current_user: UserProfile = Depends(get_current_user)):
    await get_match(mid, current_user)
    msgs = list(map(lambda msg: msg.dict(), msgs))
    if db.add_messages(msgs, mid) is False:
        raise HTTPException(status_code=status.HTTP_500_INTERNAL_SERVER_ERROR, detail="Error adding new messages")


@router.post("/{mid}/confirm", status_code=status.HTTP_200_OK)
async def confirm_meeting(mid: str, confirmation: MeetingConfirmation,
                          current_user: UserProfile = Depends(get_current_user)):
    await get_match(mid, current_user)
    confirmations = db.confirm_meeting(current_user.oid, mid)
    if confirmations is None:
        raise HTTPException(status_code=status.HTTP_500_INTERNAL_SERVER_ERROR, detail='Error confirming the meeting')

    if len(confirmations) == 2:
        confirmations.remove(current_user.oid)
        # --- Update other user
        other_user = db.get_user_by_id(confirmations[0])
        if other_user is not None:
            other_user = UserProfile(**other_user)
            # Add extra 2 matches allowance
            matches_allowed = other_user.matches_allowed
            matches_allowed += 2
            db.update_user(other_user.oid, {'matches_allowed': matches_allowed})

        # Update current user
        matches_allowed = current_user.matches_allowed
        matches_allowed += 2
        db.update_user(current_user.oid, {'matches_allowed': matches_allowed})





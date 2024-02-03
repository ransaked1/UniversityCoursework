FROM python:3.10-slim-bullseye

WORKDIR /app

RUN pip3 install poetry==1.7.1

COPY app poetry.lock pyproject.toml ./

RUN poetry config virtualenvs.create false
RUN poetry install --no-dev

ENV PYTHONUNBUFFERED=1

CMD ["uvicorn", "main:app", "--host", "0.0.0.0", "--port", "8080"]

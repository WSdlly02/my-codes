FROM python:3.14-alpine

ENV PYTHONUNBUFFERED=1

COPY requirements.txt .
RUN pip install --no-cache-dir -r requirements.txt && \
  pip cache purge
COPY resolver.py /resolver.py

EXPOSE 8088
STOPSIGNAL SIGTERM

ENTRYPOINT ["python","resolver.py"]
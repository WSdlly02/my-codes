FROM python:3.14-alpine

WORKDIR /app

ENV PYTHONUNBUFFERED=1

COPY requirements.txt .
RUN pip install --no-cache-dir -r requirements.txt

COPY resolver.py .

EXPOSE 8088

# 使用 exec 格式的 ENTRYPOINT，确保信号（如 SIGTERM）能直接传递给 Python 进程
ENTRYPOINT ["python", "resolver.py"]
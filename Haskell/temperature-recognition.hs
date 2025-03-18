-- 读取温度传感器（假设地址 0x33 为温度寄存器）
readTemperature :: IO Float
readTemperature = do
  fd <- openI2C "/dev/i2c-1"
  setSlaveAddress fd 0x33
  [high, low] <- readBlock fd 0x33 2
  let temp = (fromIntegral high * 256 + fromIntegral low) / 10.0
  return temp

main :: IO ()
main = do
  temp <- readTemperature
  let maxTemp = maximum temp
  let minTemp = minium temp
  putStrLn $ "当前温度: " ++ show temp ++ "°C" ++ "Max temp:" ++ show maxTemp ++ ",Min temp:" ++ show minTemp

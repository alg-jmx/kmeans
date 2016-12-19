GetClusterCenter = function(dataSet = NULL,
                            centers = 5,
                            iter.max = 100,
                            nstart = 20,
                            trace = F) {
  # 根据用户住址的经纬度信息进行聚类
  # Author: JuMingxing
  # Args:
  #   dataSet: data.frame[lan,lat,city], 原始数据集，包含经纬度与地址信息
  #   centers: 聚类中心个数
  #   iter.max: 最大迭代次数
  #   nstart: 如果聚类中心个数是数值，那么随机选择的nstart个数据集
  #   trace: 是否打印运行过程
  #
  # Return:
  #   返回data.frame[city,clusterType],聚类结果
  
  suppressMessages(require(dplyr))
  dataTemp = dataSet[, setdiff(colnames(dataSet), 'city')]
  kmean = kmeans(
    x = dataTemp,
    centers = centers,
    iter.max = iter.max,
    nstart = nstart,
    trace = trace
  )
  centers <- as.data.frame(kmean$centers)
  init <- arrange(centers, desc(lon), desc(lat)) # 对聚类中心做一个排序
  
  euc = function (fossite, modsite) {
    return(sqrt(sum((fossite - modsite) * (fossite - modsite)
    )))
  } # 欧氏距离的计算公式
  
  pre = function(x, init) {
    for (i in 1:nrow(init)) {
      assign(paste("x", i, sep = ""), apply(x, 1, euc, init[i,])) # 计算样本到聚类中心的欧氏距离
      if (i == 1) {
        cl = get(paste("x", i, sep = ""))
      }
      else{
        cl = cbind(cl, get(paste("x", i, sep = "")))
      }
    }
    clusterType = apply(cl, 1, which.min) # 获得分类结果
    return(clusterType)
  }
  
  clusterType = pre(dataTemp, init) # 预测的类别,确定聚类的类
  
  data.frame(name = dataSet$city, type = clusterType)
}

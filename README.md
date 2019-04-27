# shortest_distance
Fortran Coding Exercise from SYSU 2019 Grade

一、执行程序的命令行
get_shortest_path undirected_graph_info.txt shortest_path_info.txt

--》其中描述m个点n条路径无向图的undirected_graph_info.txt里面的数据格式如下:
m n
m个点的名称信息行
路径1起点 路径1终点 路径1长度
。
。
。
路径n起点 路径n终点 路径n长度
求最短路径的起点名称 求最短路径的终点名称

--》最短路径结果的文件数据内容如下:
求最短路径的起点名称 求最短路径的终点名称
是否存在最短路径
最短路径长度
最短路径的从起点到终点的点路径信息

二、关键算法：把无向图转为有向图，采用Dijkstra算法求解最短路径

三、测试案例
1. 3x6的长方形，求对角2个点的最短路线
2. SYSU理论力学专业2019年五一布置的Fortran编程题目的图形，求G点到J点的最短路径
3. SYSU理论力学专业2019年五一布置的Fortran编程题目的图形，求A点到J点的最短路径

四、使用的Fortran语言的相关命令和知识
1. 派生数据类型
2. 数组/二维数组
3. 判断和循环语句
4. 函数/子例程
5. 文件的读写
6. 访问命令行参数的内置函数

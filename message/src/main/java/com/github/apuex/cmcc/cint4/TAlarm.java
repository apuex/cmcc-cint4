package com.github.apuex.cmcc.cint4;

import java.io.Serializable;
import java.nio.ByteBuffer;

/**
 * 当前告警值的结构
 */
public class TAlarm implements Serializable {
    public String SCID; // SC ID编号（7位数字，全网范围唯一，采用6位行政地区编码+1位序号组成）
    public String SerialNo; // 告警序号（10位数字，范围0~4294967295，是下级SC范围的，不足10位前面补0）
    public String SiteID; // 站点编号
    public String DeviceID; // 设备编号
    public String SignalID; // 监控点的6位信号编码，即《动环信号标准化字典表(20170927)》中的信号编码ID
    public String SignalNumber; // 同类监控点顺序号
    public String NMAlarmID; // 6位告警编码ID
    public String AlarmTime; // 告警时间，YYYY-MM-DD<SPACE键>hh:mm:ss（采用24小时的时间制式）
    public EnumState AlarmLevel; // 数据值的状态
    public EnumFlag AlarmFlag; // 告警标志
    public float EventValue; // 告警触发值
    public String AlarmDesc; // 告警描述
}


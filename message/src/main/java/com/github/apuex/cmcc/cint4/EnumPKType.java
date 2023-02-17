package com.github.apuex.cmcc.cint4;

/**
 * 报文定义
 */
public enum EnumPKType {
    LOGIN(101) //登录
    , LOGIN_ACK(102) //登录响应
    , LOGOUT(103) //登出
    , LOGOUT_ACK(104) //登出响应
    , SET_DYN_ACCESS_MODE(401) //请求实时数据方式设置
    , DYN_ACCESS_MODE_ACK(402) //实时数据响应
    , SET_ALARM_MODE(501) //请求告警数据方式设置
    , ALARM_MODE_ACK(502) //告警方式设置响应
    , SEND_ALARM(503) //实时告警发送
    , SEND_ALARM_ACK(504) //实时告警发送确认
    , SYNC_ALARM(505) //告警同步
    , SYNC_ALARM_ACK(506) //告警同步确认
    , SET_POINT(1001) //写数据请求
    , SET_POINT_ACK(1002) //写数据响应
    , MODIFY_PA(1101) //改口令请求
    , MODIFY_PA_ACK(1102) //改口令响应
    , HEART_BEAT(1201) //确认连接
    , HEART_BEAT_ACK(1202) //回应连接
    , TIME_CHECK(1301) //发送时钟消息
    , TIME_CHECK_ACK(1302) //时钟同步响应
    ;

    EnumPKType(int v) {
        this.value = v;
    }

    public int getValue() {
        return this.value;
    }

    public static EnumPKType fromValue(int v) {
        switch(v) {
        case 101: return LOGIN;
        case 102: return LOGIN_ACK;
        case 103: return LOGOUT;
        case 104: return LOGOUT_ACK;
        case 401: return SET_DYN_ACCESS_MODE;
        case 402: return DYN_ACCESS_MODE_ACK;
        case 501: return SET_ALARM_MODE;
        case 502: return ALARM_MODE_ACK;
        case 503: return SEND_ALARM;
        case 504: return SEND_ALARM_ACK;
        case 505: return SYNC_ALARM;
        case 506: return SYNC_ALARM_ACK;
        case 1001: return SET_POINT;
        case 1002: return SET_POINT_ACK;
        case 1101: return MODIFY_PA;
        case 1102: return MODIFY_PA_ACK;
        case 1201: return HEART_BEAT;
        case 1202: return HEART_BEAT_ACK;
        case 1301: return TIME_CHECK;
        case 1302: return TIME_CHECK_ACK;
        default: throw new IllegalArgumentException(String.format("%d is an invalid enum value.", v));
        }
    }
 
    private final int value;
}

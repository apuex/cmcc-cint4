�{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE DeriveGeneric #-}
module MessageModel where

import qualified Metadata               as Meta
import qualified Data.List              as L
import qualified Data.Text              as T

-- Enums
enumRightLevel = Meta.EnumerateField
    { Meta.fieldName    = "EnumRightLevel"
    , Meta.fieldType    = "int32_t"
    , Meta.fieldSize    = ""
    , Meta.fieldValue   = ""
    , Meta.fieldEnums   =
        [ Meta.EnumItem { Meta.enumName = "INVALID"
                        , Meta.enumValue = 0
                        , Meta.enumComment = "无权限"
                        }
        , Meta.EnumItem { Meta.enumName = "LEVEL1"
                        , Meta.enumValue = 1
                        , Meta.enumComment = "具备数据读的权限,当用户可以读某个数据，而无法写任何数据时返回这一权限值。"
                        }
        , Meta.EnumItem { Meta.enumName = "LEVEL2"
                        , Meta.enumValue = 2
                        , Meta.enumComment = "具备数据读、写的权限，当用户对某个数据具有读写权限时返回这一权限值。"
                        }
        ]
    , Meta.fieldComment = "监控系统下级SC向上级SC提供的权限定义"
    }

enumResult = Meta.EnumerateField
    { Meta.fieldName    = "EnumResult"
    , Meta.fieldType    = "int32_t"
    , Meta.fieldSize    = ""
    , Meta.fieldValue   = ""
    , Meta.fieldEnums   =
        [ Meta.EnumItem { Meta.enumName = "FAILURE"
                        , Meta.enumValue = 0
                        , Meta.enumComment = "失败"
                        }
        , Meta.EnumItem { Meta.enumName = "SUCCESS"
                        , Meta.enumValue = 1
                        , Meta.enumComment = "成功"
                        }
        ]
    , Meta.fieldComment = "报文返回结果"
    }

enumType = Meta.EnumerateField
    { Meta.fieldName    = "EnumType"
    , Meta.fieldType    = "int32_t"
    , Meta.fieldSize    = ""
    , Meta.fieldValue   = ""
    , Meta.fieldEnums   =
        [ Meta.EnumItem { Meta.enumName = "ALARM"
                        , Meta.enumValue = 0
                        , Meta.enumComment = "告警"
                        }
        , Meta.EnumItem { Meta.enumName = "DO"
                        , Meta.enumValue = 1
                        , Meta.enumComment = "数字输出量，遥控"
                        }
        , Meta.EnumItem { Meta.enumName = "AO"
                        , Meta.enumValue = 2
                        , Meta.enumComment = "模拟输出量，遥调"
                        }
        , Meta.EnumItem { Meta.enumName = "AI"
                        , Meta.enumValue = 3
                        , Meta.enumComment = "模拟输入量，遥测"
                        }
        , Meta.EnumItem { Meta.enumName = "DI"
                        , Meta.enumValue = 4
                        , Meta.enumComment = "数字输入量（包含多态数字输入量），遥信"
                        }
        , Meta.EnumItem { Meta.enumName = "DEVICE"
                        , Meta.enumValue = 5
                        , Meta.enumComment = "设备"
                        }
        , Meta.EnumItem { Meta.enumName = "ROOM"
                        , Meta.enumValue = 6
                        , Meta.enumComment = "机房"
                        }
        , Meta.EnumItem { Meta.enumName = "SITE"
                        , Meta.enumValue = 7
                        , Meta.enumComment = "站点"
                        }
        , Meta.EnumItem { Meta.enumName = "AREA"
                        , Meta.enumValue = 8
                        , Meta.enumComment = "区域"
                        }
        ]
    , Meta.fieldComment = "监控系统数据的种类"
    }

enumAlarmLevel = Meta.EnumerateField
    { Meta.fieldName    = "EnumAlarmLevel"
    , Meta.fieldType    = "int32_t"
    , Meta.fieldSize    = ""
    , Meta.fieldValue   = ""
    , Meta.fieldEnums   =
        [ Meta.EnumItem { Meta.enumName = "NOALARM"
                        , Meta.enumValue = 0
                        , Meta.enumComment = "无告警"
                        }
        , Meta.EnumItem { Meta.enumName = "CRITICAL"
                        , Meta.enumValue = 1
                        , Meta.enumComment = "一级告警"
                        }
        , Meta.EnumItem { Meta.enumName = "MAJOR"
                        , Meta.enumValue = 2
                        , Meta.enumComment = "二级告警"
                        }
        , Meta.EnumItem { Meta.enumName = "MINOR"
                        , Meta.enumValue = 3
                        , Meta.enumComment = "三级告警"
                        }
        , Meta.EnumItem { Meta.enumName = "HINT"
                        , Meta.enumValue = 4
                        , Meta.enumComment = "四级告警"
                        }
        ]
    , Meta.fieldComment = "告警的等级"
    }

enumEnable = Meta.EnumerateField
    { Meta.fieldName    = "EnumEnable"
    , Meta.fieldType    = "int32_t"
    , Meta.fieldSize    = ""
    , Meta.fieldValue   = ""
    , Meta.fieldEnums   =
        [ Meta.EnumItem { Meta.enumName = "DISABLE"
                        , Meta.enumValue = 0
                        , Meta.enumComment = "禁止/不能"
                        }
        , Meta.EnumItem { Meta.enumName = "ENABLE"
                        , Meta.enumValue = 1
                        , Meta.enumComment = "开放/能"
                        }
        ]
    , Meta.fieldComment = "使能的属性"
    }

enumFlag = Meta.EnumerateField
    { Meta.fieldName    = "EnumFlag"
    , Meta.fieldType    = "int32_t"
    , Meta.fieldSize    = ""
    , Meta.fieldValue   = ""
    , Meta.fieldEnums   =
        [ Meta.EnumItem { Meta.enumName = "BEGIN"
                        , Meta.enumValue = 0
                        , Meta.enumComment = "开始"
                        }
        , Meta.EnumItem { Meta.enumName = "END"
                        , Meta.enumValue = 1
                        , Meta.enumComment = "结束"
                        }
        ]
    , Meta.fieldComment = "告警标志"
    }

enumAccessMode = Meta.EnumerateField
    { Meta.fieldName    = "EnumAccessMode"
    , Meta.fieldType    = "int32_t"
    , Meta.fieldSize    = ""
    , Meta.fieldValue   = ""
    , Meta.fieldEnums   =
        [ Meta.EnumItem { Meta.enumName = "ASK_ANSWER"
                        , Meta.enumValue = 0
                        , Meta.enumComment = "一问一答方式"
                        }
        , Meta.EnumItem { Meta.enumName = "CHANGE_TRIGGER"
                        , Meta.enumValue = 1
                        , Meta.enumComment = "改变时自动发送数据方式"
                        }
        , Meta.EnumItem { Meta.enumName = "TIME_TRIGGER"
                        , Meta.enumValue = 2
                        , Meta.enumComment = "定时发送数据方式"
                        }
        , Meta.EnumItem { Meta.enumName = "STOP"
                        , Meta.enumValue = 3
                        , Meta.enumComment = "停止发送数据方式"
                        }
        ]
    , Meta.fieldComment = "下级实时数据访问的方式"
    }

enumState = Meta.EnumerateField
    { Meta.fieldName    = "Status"
    , Meta.fieldType    = "EnumState"
    , Meta.fieldSize    = ""
    , Meta.fieldValue   = ""
    , Meta.fieldEnums   =
        [ Meta.EnumItem { Meta.enumName = "NOALARM"
                        , Meta.enumValue = 0
                        , Meta.enumComment = "正常数据"
                        }
        , Meta.EnumItem { Meta.enumName = "CRITICAL"
                        , Meta.enumValue = 1
                        , Meta.enumComment = "一级告警"
                        }
        , Meta.EnumItem { Meta.enumName = "MAJOR"
                        , Meta.enumValue = 2
                        , Meta.enumComment = "二级告警"
                        }
        , Meta.EnumItem { Meta.enumName = "MINOR"
                        , Meta.enumValue = 3
                        , Meta.enumComment = "三级告警"
                        }
        , Meta.EnumItem { Meta.enumName = "HINT"
                        , Meta.enumValue = 4
                        , Meta.enumComment = "四级告警"
                        }
        , Meta.EnumItem { Meta.enumName = "OPEVENT"
                        , Meta.enumValue = 5
                        , Meta.enumComment = "操作事件"
                        }
        , Meta.EnumItem { Meta.enumName = "INVALID"
                        , Meta.enumValue = 6
                        , Meta.enumComment = "无效数据"
                        }
        ]
    , Meta.fieldComment = "数据值的状态"
    }

enumAlarmMode = Meta.EnumerateField
    { Meta.fieldName    = "EnumAlarmMode"
    , Meta.fieldType    = "int32_t"
    , Meta.fieldSize    = ""
    , Meta.fieldValue   = ""
    , Meta.fieldEnums   =
        [ Meta.EnumItem { Meta.enumName = "NOALARM"
                        , Meta.enumValue = 0
                        , Meta.enumComment = "不做告警上报"
                        }
        , Meta.EnumItem { Meta.enumName = "CRITICAL"
                        , Meta.enumValue = 1
                        , Meta.enumComment = "一级告警上报"
                        }
        , Meta.EnumItem { Meta.enumName = "MAJOR"
                        , Meta.enumValue = 2
                        , Meta.enumComment = "二级告警上报"
                        }
        , Meta.EnumItem { Meta.enumName = "MINOR"
                        , Meta.enumValue = 3
                        , Meta.enumComment = "三级告警上报"
                        }
        , Meta.EnumItem { Meta.enumName = "HINT"
                        , Meta.enumValue = 4
                        , Meta.enumComment = "四级告警上报"
                        }
        ]
    , Meta.fieldComment = "告警等级设定的模式"
    }

enumSiteType = Meta.EnumerateField
    { Meta.fieldName    = "EnumSiteType"
    , Meta.fieldType    = "int32_t"
    , Meta.fieldSize    = ""
    , Meta.fieldValue   = ""
    , Meta.fieldEnums   =
        [ Meta.EnumItem { Meta.enumName = "DATACENTER"
                        , Meta.enumValue = 1
                        , Meta.enumComment = "数据中心"
                        }
        , Meta.EnumItem { Meta.enumName = "ROOM"
                        , Meta.enumValue = 2
                        , Meta.enumComment = "通信机楼"
                        }
        , Meta.EnumItem { Meta.enumName = "LOCALTRANS"
                        , Meta.enumValue = 3
                        , Meta.enumComment = "传输节点"
                        }
        , Meta.EnumItem { Meta.enumName = "STATION"
                        , Meta.enumValue = 4
                        , Meta.enumComment = "通信基站"
                        }
        ]
    , Meta.fieldComment = "局站类型"
    }

enumRoomType = Meta.EnumerateField
    { Meta.fieldName    = "EnumRoomType"
    , Meta.fieldType    = "int32_t"
    , Meta.fieldSize    = ""
    , Meta.fieldValue   = ""
    , Meta.fieldEnums   =
        [ Meta.EnumItem { Meta.enumName = "CONVERGE"
                        , Meta.enumValue = 1
                        , Meta.enumComment = "汇聚机房"
                        }
        , Meta.EnumItem { Meta.enumName = "BASESTATION"
                        , Meta.enumValue = 2
                        , Meta.enumComment = "基站机房"
                        }
        , Meta.EnumItem { Meta.enumName = "GENERATION"
                        , Meta.enumValue = 11
                        , Meta.enumComment = "发电机房"
                        }
        , Meta.EnumItem { Meta.enumName = "ELECTRIC"
                        , Meta.enumValue = 12
                        , Meta.enumComment = "电力机房"
                        }
        , Meta.EnumItem { Meta.enumName = "BATTERY"
                        , Meta.enumValue = 13
                        , Meta.enumComment = "电池机房"
                        }
        , Meta.EnumItem { Meta.enumName = "AIRCONDITION"
                        , Meta.enumValue = 14
                        , Meta.enumComment = "空调机房"
                        }
        , Meta.EnumItem { Meta.enumName = "TRANSFERS"
                        , Meta.enumValue = 51
                        , Meta.enumComment = "传输机房"
                        }
        , Meta.EnumItem { Meta.enumName = "EXCHANGE"
                        , Meta.enumValue = 52
                        , Meta.enumComment = "交换机房"
                        }
        , Meta.EnumItem { Meta.enumName = "DATA"
                        , Meta.enumValue = 53
                        , Meta.enumComment = "数据机房"
                        }
        , Meta.EnumItem { Meta.enumName = "IDC"
                        , Meta.enumValue = 54
                        , Meta.enumComment = "IDC机房"
                        }
        , Meta.EnumItem { Meta.enumName = "COLLIGATION"
                        , Meta.enumValue = 54
                        , Meta.enumComment = "综合机房"
                        }
        ]
    , Meta.fieldComment = "机房类型"
    }

enumModifyType = Meta.EnumerateField
    { Meta.fieldName    = "EnumModifyType"
    , Meta.fieldType    = "int32_t"
    , Meta.fieldSize    = ""
    , Meta.fieldValue   = ""
    , Meta.fieldEnums   =
        [ Meta.EnumItem { Meta.enumName = "ADDNONODES"
                        , Meta.enumValue = 0
                        , Meta.enumComment = "新增（无子节点）"
                        }
        , Meta.EnumItem { Meta.enumName = "ADDINNODES"
                        , Meta.enumValue = 1
                        , Meta.enumComment = "新增（含子节点）"
                        }
        , Meta.EnumItem { Meta.enumName = "DELETE"
                        , Meta.enumValue = 2
                        , Meta.enumComment = "删除"
                        }
        , Meta.EnumItem { Meta.enumName = "MODIFYNONODES"
                        , Meta.enumValue = 3
                        , Meta.enumComment = "修改（仅修改本节点）"
                        }
        , Meta.EnumItem { Meta.enumName = "MODIFYINNODES"
                        , Meta.enumValue = 4
                        , Meta.enumComment = "修改（涉及到子节点）"
                        }
        ]
    , Meta.fieldComment = "对象属性修改类型"
    }

enumUpdateType = Meta.EnumerateField
    { Meta.fieldName    = "EnumUpdateType"
    , Meta.fieldType    = "int32_t"
    , Meta.fieldSize    = ""
    , Meta.fieldValue   = ""
    , Meta.fieldEnums   =
        [ Meta.EnumItem { Meta.enumName = "StoragePeriod"
                        , Meta.enumValue = 0
                        , Meta.enumComment = "存储周期"
                        }
        , Meta.EnumItem { Meta.enumName = "Absolute"
                        , Meta.enumValue = 1
                        , Meta.enumComment = "绝对阀值"
                        }
        , Meta.EnumItem { Meta.enumName = "Relative"
                        , Meta.enumValue = 2
                        , Meta.enumComment = "相对阀值"
                        }
        , Meta.EnumItem { Meta.enumName = "Static"
                        , Meta.enumValue = 3
                        , Meta.enumComment = "统计标志"
                        }
        ]
    , Meta.fieldComment = "数据更新类型"
    }

enumNodeType = Meta.EnumerateField
    { Meta.fieldName    = "EnumNodeType"
    , Meta.fieldType    = "int32_t"
    , Meta.fieldSize    = ""
    , Meta.fieldValue   = ""
    , Meta.fieldEnums   =
        [ Meta.EnumItem { Meta.enumName = "NodeType0"
                        , Meta.enumValue = 0
                        , Meta.enumComment = "M-GSM900M"
                        }
        , Meta.EnumItem { Meta.enumName = "NodeType1"
                        , Meta.enumValue = 1
                        , Meta.enumComment = "D-DCS1800M"
                        }
        , Meta.EnumItem { Meta.enumName = "NodeType2"
                        , Meta.enumValue = 2
                        , Meta.enumComment = "T-DSCDMA"
                        }
        , Meta.EnumItem { Meta.enumName = "NodeType3"
                        , Meta.enumValue = 3
                        , Meta.enumComment = "LTE"
                        }
        ]
    , Meta.fieldComment = "局站特征逻辑分类"
    }

enumNeStatusType = Meta.EnumerateField
    { Meta.fieldName    = "EnumNeStatusType"
    , Meta.fieldType    = "int32_t"
    , Meta.fieldSize    = ""
    , Meta.fieldValue   = ""
    , Meta.fieldEnums   =
        [ Meta.EnumItem { Meta.enumName = "NeStatusType1"
                        , Meta.enumValue = 1
                        , Meta.enumComment = "现网有业务承载"
                        }
        , Meta.EnumItem { Meta.enumName = "NeStatusType2"
                        , Meta.enumValue = 2
                        , Meta.enumComment = "现网无业务承载"
                        }
        , Meta.EnumItem { Meta.enumName = "NeStatusType3"
                        , Meta.enumValue = 3
                        , Meta.enumComment = "退网"
                        }
        , Meta.EnumItem { Meta.enumName = "NeStatusType4"
                        , Meta.enumValue = 4
                        , Meta.enumComment = "工程"
                        }
        , Meta.EnumItem { Meta.enumName = "NeStatusType5"
                        , Meta.enumValue = 5
                        , Meta.enumComment = "删除"
                        }
        , Meta.EnumItem { Meta.enumName = "NeStatusType6"
                        , Meta.enumValue = 6
                        , Meta.enumComment = "未确认"
                        }
        ]
    , Meta.fieldComment = "资源工程状态"
    }

enumPKType = Meta.EnumerateField
    { Meta.fieldName    = "PKType"
    , Meta.fieldType    = "EnumPKType"
    , Meta.fieldSize    = ""
    , Meta.fieldValue   = ""
    , Meta.fieldEnums   =
        [ Meta.EnumItem { Meta.enumName = "LOGIN"
                        , Meta.enumValue = 101
                        , Meta.enumComment = "登录"
                        }
        , Meta.EnumItem { Meta.enumName = "LOGIN_ACK"
                        , Meta.enumValue = 102
                        , Meta.enumComment = "登录响应"
                        }
        , Meta.EnumItem { Meta.enumName = "LOGOUT"
                        , Meta.enumValue = 103
                        , Meta.enumComment = "登出"
                        }
        , Meta.EnumItem { Meta.enumName = "LOGOUT_ACK"
                        , Meta.enumValue = 104
                        , Meta.enumComment = "登出响应"
                        }
        , Meta.EnumItem { Meta.enumName = "SET_DYN_ACCESS_MODE"
                        , Meta.enumValue = 401
                        , Meta.enumComment = "请求实时数据方式设置"
                        }
        , Meta.EnumItem { Meta.enumName = "DYN_ACCESS_MODE_ACK"
                        , Meta.enumValue = 402
                        , Meta.enumComment = "实时数据响应"
                        }
        , Meta.EnumItem { Meta.enumName = "SET_ALARM_MODE"
                        , Meta.enumValue = 501
                        , Meta.enumComment = "请求告警数据方式设置"
                        }
        , Meta.EnumItem { Meta.enumName = "ALARM_MODE_ACK"
                        , Meta.enumValue = 502
                        , Meta.enumComment = "告警方式设置响应"
                        }
        , Meta.EnumItem { Meta.enumName = "SEND_ALARM"
                        , Meta.enumValue = 503
                        , Meta.enumComment = "实时告警发送"
                        }
        , Meta.EnumItem { Meta.enumName = "SEND_ALARM_ACK"
                        , Meta.enumValue = 504
                        , Meta.enumComment = "实时告警发送确认"
                        }
        , Meta.EnumItem { Meta.enumName = "SYNC_ALARM"
                        , Meta.enumValue = 505
                        , Meta.enumComment = "告警同步"
                        }
        , Meta.EnumItem { Meta.enumName = "SYNC_ALARM_ACK"
                        , Meta.enumValue = 506
                        , Meta.enumComment = "告警同步确认"
                        }
        , Meta.EnumItem { Meta.enumName = "SET_POINT"
                        , Meta.enumValue = 1001
                        , Meta.enumComment = "写数据请求"
                        }
        , Meta.EnumItem { Meta.enumName = "SET_POINT_ACK"
                        , Meta.enumValue = 1002
                        , Meta.enumComment = "写数据响应"
                        }
        , Meta.EnumItem { Meta.enumName = "MODIFY_PA"
                        , Meta.enumValue = 1101
                        , Meta.enumComment = "改口令请求"
                        }
        , Meta.EnumItem { Meta.enumName = "MODIFY_PA_ACK"
                        , Meta.enumValue = 1102
                        , Meta.enumComment = "改口令响应"
                        }
        , Meta.EnumItem { Meta.enumName = "HEART_BEAT"
                        , Meta.enumValue = 1201
                        , Meta.enumComment = "确认连接"
                        }
        , Meta.EnumItem { Meta.enumName = "HEART_BEAT_ACK"
                        , Meta.enumValue = 1202
                        , Meta.enumComment = "回应连接"
                        }
        , Meta.EnumItem { Meta.enumName = "TIME_CHECK"
                        , Meta.enumValue = 1301
                        , Meta.enumComment = "发送时钟消息"
                        }
        , Meta.EnumItem { Meta.enumName = "TIME_CHECK_ACK"
                        , Meta.enumValue = 1302
                        , Meta.enumComment = "时钟同步响应"
                        }
        ]
    , Meta.fieldComment = "报文定义"
    }


-- Data Structure Fields

-- TTime
years = Meta.Int16Field
    { Meta.fieldName    = "Years"
    , Meta.fieldSize    = ""
    , Meta.fieldValue   = ""
    , Meta.fieldComment = "年"
    }

month = Meta.Int8Field
    { Meta.fieldName    = "Month"
    , Meta.fieldSize    = ""
    , Meta.fieldValue   = ""
    , Meta.fieldComment = "月"
    }

day = Meta.Int8Field
    { Meta.fieldName    = "Day"
    , Meta.fieldSize    = ""
    , Meta.fieldValue   = ""
    , Meta.fieldComment = "日"
    }

hour = Meta.Int8Field
    { Meta.fieldName    = "Hour"
    , Meta.fieldSize    = ""
    , Meta.fieldValue   = ""
    , Meta.fieldComment = "时"
    }

minute = Meta.Int8Field
    { Meta.fieldName    = "Minute"
    , Meta.fieldSize    = ""
    , Meta.fieldValue   = ""
    , Meta.fieldComment = "分"
    }

second = Meta.Int8Field
    { Meta.fieldName    = "Second"
    , Meta.fieldSize    = ""
    , Meta.fieldValue   = ""
    , Meta.fieldComment = "秒"
    }

tTime = Meta.Struct
    { Meta.entityId      = ""
    , Meta.entityName    = "TTime"
    , Meta.entityComment = "时间的结构"
    , Meta.entityFields  =
        [ years
        , month
        , day
        , hour
        , minute
        , second
        ]
    }

-- TA/TD/TID
siteID = Meta.StringField
    { Meta.fieldName    = "SiteID"
    , Meta.fieldSize    = "Length::SITEID_LEN"
    , Meta.fieldValue   = ""
    , Meta.fieldComment = "站点编号"
    }

deviceID = Meta.StringField
    { Meta.fieldName    = "DeviceID"
    , Meta.fieldSize    = "Length::DEVICEID_LEN"
    , Meta.fieldValue   = ""
    , Meta.fieldComment = "设备编号"
    }

signalID = Meta.StringField
    { Meta.fieldName    = "SignalID"
    , Meta.fieldSize    = "Length::ID_LEN"
    , Meta.fieldValue   = ""
    , Meta.fieldComment = "监控点的6位信号编码，即《动环信号标准化字典表(20170927)》中的信号编码ID"
    }

signalNumber = Meta.StringField
    { Meta.fieldName    = "SignalNumber"
    , Meta.fieldSize    = "Length::SIGNALNUM_LEN"
    , Meta.fieldValue   = ""
    , Meta.fieldComment = "同类监控点顺序号"
    }

aiValue = Meta.StringField
    { Meta.fieldName    = "Value"
    , Meta.fieldSize    = ""
    , Meta.fieldValue   = ""
    , Meta.fieldComment = "AI值"
    }

ta = Meta.Struct
    { Meta.entityId      = ""
    , Meta.entityName    = "TA"
    , Meta.entityComment = "模拟量的值的结构"
    , Meta.entityFields  =
        [ siteID
        , deviceID
        , signalID
        , signalNumber
        , aiValue
        , enumState
        ]
    }

diValue = Meta.StringField
    { Meta.fieldName    = "Value"
    , Meta.fieldSize    = ""
    , Meta.fieldValue   = ""
    , Meta.fieldComment = "DI值"
    }

td = Meta.Struct
    { Meta.entityId      = ""
    , Meta.entityName    = "TD"
    , Meta.entityComment = "数字量的值的结构"
    , Meta.entityFields  =
        [ siteID
        , deviceID
        , signalID
        , signalNumber
        , diValue
        , enumState
        ]
    }

tid = Meta.Struct
    { Meta.entityId      = ""
    , Meta.entityName    = "TID"
    , Meta.entityComment = "数据值的结构"
    , Meta.entityFields  =
        [ siteID
        , deviceID
        , signalID
        , signalNumber
        ]
    }


-- TAlarm
scID = Meta.StringField
    { Meta.fieldName    = "SCID"
    , Meta.fieldSize    = "SCID_LEN"
    , Meta.fieldValue   = ""
    , Meta.fieldComment = "SC ID编号（7位数字，全网范围唯一，采用6位行政地区编码+1位序号组成）"
    }

serialNo = Meta.StringField
    { Meta.fieldName    = "SerialNo"
    , Meta.fieldSize    = "ALARMSERIALNO_LEN"
    , Meta.fieldValue   = ""
    , Meta.fieldComment = "告警序号（10位数字，范围0~4294967295，是下级SC范围的，不足10位前面补0）"
    }

nmAlarmID = Meta.StringField
    { Meta.fieldName    = "NMAlarmID"
    , Meta.fieldSize    = "ID_LEN"
    , Meta.fieldValue   = ""
    , Meta.fieldComment = "6位告警编码ID"
    }

alarmTime = Meta.StringField
    { Meta.fieldName    = "AlarmTime"
    , Meta.fieldSize    = "TIME_LEN"
    , Meta.fieldValue   = ""
    , Meta.fieldComment = "告警时间，YYYY-MM-DD<SPACE键>hh:mm:ss（采用24小时的时间制式）"
    }

eventValue = Meta.Float32Field
    { Meta.fieldName    = "EventValue"
    , Meta.fieldValue   = ""
    , Meta.fieldComment = "告警触发值"
    }

alarmDesc = Meta.StringField
    { Meta.fieldName    = "AlarmDesc"
    , Meta.fieldSize    = "DES_LENGTH"
    , Meta.fieldValue   = ""
    , Meta.fieldComment = "告警描述"
    }

tAlarm = Meta.Struct
    { Meta.entityId      = ""
    , Meta.entityName    = "TAlarm"
    , Meta.entityComment = "当前告警值的结构"
    , Meta.entityFields  =
        [ scID
        , serialNo
        , siteID
        , deviceID
        , signalID
        , signalNumber
        , nmAlarmID
        , alarmTime
        , enumState { Meta.fieldName = "AlarmLevel" }
        , enumFlag { Meta.fieldName = "AlarmFlag" }
        , eventValue
        , alarmDesc
        ]
    }


-- Info Fields
userName = Meta.StringField
    { Meta.fieldName    = "UserName"
    , Meta.fieldSize    = "USER_LENGTH"
    , Meta.fieldValue   = ""
    , Meta.fieldComment = "Login user name."
    }

passWord = Meta.StringField
    { Meta.fieldName    = "PassWord"
    , Meta.fieldSize    = "PASSWORD_LEN"
    , Meta.fieldValue   = ""
    , Meta.fieldComment = "Login password."
    }

-- Message header and tail CRC16 fields

msgHeader = Meta.Int32Field
    { Meta.fieldName    = "Header"
    , Meta.fieldSize    = ""
    , Meta.fieldValue   = "0x7E7C6B5A"
    , Meta.fieldComment = "Message header, 0x7E7C6B5A."
    }

msgLength = Meta.Int32Field
    { Meta.fieldName    = "Length"
    , Meta.fieldSize    = ""
    , Meta.fieldValue   = ""
    , Meta.fieldComment = "Message length."
    }

msgSerialNo = Meta.Int32Field
    { Meta.fieldName    = "SerialNo"
    , Meta.fieldSize    = ""
    , Meta.fieldValue   = ""
    , Meta.fieldComment = "Message serial number."
    }

msgPKType = Meta.Int32Field
    { Meta.fieldName    = "PKType"
    , Meta.fieldSize    = ""
    , Meta.fieldValue   = ""
    , Meta.fieldComment = "PKType."
    }

msgCRC16 = Meta.UInt16Field
    { Meta.fieldName    = "CRC16"
    , Meta.fieldSize    = ""
    , Meta.fieldValue   = ""
    , Meta.fieldComment = "Message CRC16."
    }

-- messages
heartBeat = Meta.Message
    { Meta.entityId      = "Message::HEART_BEAT"
    , Meta.entityName   = "HeartBeat"
    , Meta.entityComment = "当前告警值的结构"
    , Meta.entityFields = []
    }

login = Meta.Message
    { Meta.entityId      = "Message::LOGIN"
    , Meta.entityName   = "Login"
    , Meta.entityComment = "当前告警值的结构"
    , Meta.entityFields =
        [ userName
        , passWord
        ]
    }

loginAck = Meta.Message
    { Meta.entityId      = "Message::LOGIN_ACK"
    , Meta.entityName   = "LoginAck"
    , Meta.entityComment = "当前告警值的结构"
    , Meta.entityFields =
        [ enumRightLevel
        ]
    }

logout = Meta.Message
    { Meta.entityId      = "Message::LOGOUT"
    , Meta.entityName   = "Logout"
    , Meta.entityComment = "当前告警值的结构"
    , Meta.entityFields = []
    }

logoutAck = Meta.Message
    { Meta.entityId      = "Message::LOGOUT_ACK"
    , Meta.entityName   = "LogoutAck"
    , Meta.entityComment = "当前告警值的结构"
    , Meta.entityFields = []
    }


model = Meta.Model
    { Meta.namespace = "message"
    , Meta.headerFields = 
        [ msgHeader
        , msgLength
        , msgSerialNo
        , msgPKType
        ]
    , Meta.tailFields = 
        [ msgCRC16
        ]
    , Meta.entities =
        [ heartBeat
        , login, loginAck
        , logout, logoutAck
        ]
    , Meta.enumerates =
        [ enumRightLevel
        , enumResult
        , enumType
        , enumAlarmLevel
        , enumEnable
        , enumFlag
        , enumAccessMode
        , enumState
        , enumAlarmMode
        , enumSiteType
        , enumRoomType
        , enumModifyType
        , enumUpdateType
        , enumNodeType
        , enumNeStatusType
        , enumPKType        ]
    }

